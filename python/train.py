import torch
import torch.nn.functional as F
import numpy as np
import os
import pickle
import pandas as pd
from datetime import datetime
from gnn_model import TeamGNN, MatchupPredictor

_DEFAULT_SAVE_DIR = os.path.normpath(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data", "models")
)


def to_records(matchup_labels):
    df = pd.DataFrame(matchup_labels)
    return df.to_dict(orient="records")


def evaluate(gnn, pred, records, game_feats_a, game_feats_b,
             x, edge_index, edge_attr):
    gnn.eval(); pred.eval()

    with torch.no_grad():
        embeddings = gnn(x, edge_index, edge_attr)
        idx_a = torch.tensor([int(r["team_a_idx"]) for r in records],
                              dtype=torch.long)
        idx_b = torch.tensor([int(r["team_b_idx"]) for r in records],
                              dtype=torch.long)
        fa_proj, fb_proj = pred.project_feats(
            torch.tensor(np.array(game_feats_a), dtype=torch.float),
            torch.tensor(np.array(game_feats_b), dtype=torch.float)
        )
        ea = embeddings[idx_a] + fa_proj
        eb = embeddings[idx_b] + fb_proj

        neutral = torch.tensor(
            [[float(r["neutral"])] for r in records],
            dtype=torch.float
        )
        pred_sa, pred_sb = pred(ea, eb, neutral)
        pred_sa = pred_sa.numpy()
        pred_sb = pred_sb.numpy()

    actuals_sa = np.array([float(r["score_a"]) for r in records])
    actuals_sb = np.array([float(r["score_b"]) for r in records])
    actuals_wp = np.array([float(r["winner"])   for r in records])

    mae_a  = float(np.mean(np.abs(pred_sa - actuals_sa)))
    mae_b  = float(np.mean(np.abs(pred_sb - actuals_sb)))
    mae    = (mae_a + mae_b) / 2
    rmse_a = float(np.sqrt(np.mean((pred_sa - actuals_sa) ** 2)))
    rmse_b = float(np.sqrt(np.mean((pred_sb - actuals_sb) ** 2)))
    rmse   = (rmse_a + rmse_b) / 2

    pred_winner   = (pred_sa > pred_sb).astype(int)
    accuracy      = float(np.mean(pred_winner == actuals_wp))
    pred_margin   = pred_sa - pred_sb
    actual_margin = actuals_sa - actuals_sb
    mae_margin    = float(np.mean(np.abs(pred_margin - actual_margin)))
    direction_acc = float(np.mean(
        np.sign(pred_margin) == np.sign(actual_margin)
    ))
    implied_wp = 1 / (1 + np.exp(-pred_margin / 7.0))
    brier      = float(np.mean((implied_wp - actuals_wp) ** 2))

    return {
        "accuracy":      accuracy,
        "direction_acc": direction_acc,
        "mae_points":    mae,
        "mae_home":      mae_a,
        "mae_away":      mae_b,
        "rmse_points":   rmse,
        "mae_margin":    mae_margin,
        "brier_score":   brier,
        "n_games":       len(records)
    }


def train(node_features, edge_src, edge_dst, edge_weights,
          team_names, matchup_labels, game_feats_a, game_feats_b,
          recency_weights, half_life_days=60.0, epochs=500, lr=5e-4,
          test_fraction=0.2, save_dir=None):

    if save_dir is None:
        save_dir = _DEFAULT_SAVE_DIR

    os.makedirs(save_dir, exist_ok=True)

    x           = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index  = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr   = torch.tensor(list(edge_weights), dtype=torch.float)
    in_channels = x.shape[1]

    records     = to_records(matchup_labels)
    rec_weights = list(recency_weights)

    fa_all   = np.array(game_feats_a)
    fb_all   = np.array(game_feats_b)
    feat_dim = fa_all.shape[1]

    print(f"DEBUG: in_channels={in_channels}, feat_dim={feat_dim}")

    # ── Chronological train/test split ────────────────────────────────────────
    n_total = len(records)
    n_train = int(n_total * (1 - test_fraction))

    train_records = records[:n_train]
    test_records  = records[n_train:]
    train_fa      = fa_all[:n_train]
    train_fb      = fb_all[:n_train]
    test_fa       = fa_all[n_train:]
    test_fb       = fb_all[n_train:]
    train_weights = torch.tensor(rec_weights[:n_train], dtype=torch.float)

    print(f"Train: {len(train_records)} games | "
          f"Test: {len(test_records)} games | "
          f"half-life={half_life_days}d | epochs={epochs}")
    print(f"Saving to: {save_dir}")

    # ── Models ────────────────────────────────────────────────────────────────
    gnn  = TeamGNN(in_channels)
    pred = MatchupPredictor(embed_dim=32, hidden=64, feat_dim=feat_dim)
    optimizer = torch.optim.Adam(
        list(gnn.parameters()) + list(pred.parameters()),
        lr=lr, weight_decay=1e-4
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs)
    huber = torch.nn.HuberLoss(reduction="none", delta=10.0)

    for epoch in range(epochs):
        gnn.train(); pred.train()
        optimizer.zero_grad()

        embeddings = gnn(x, edge_index, edge_attr)

        fa_t  = torch.tensor(train_fa, dtype=torch.float)
        fb_t  = torch.tensor(train_fb, dtype=torch.float)
        idx_a = torch.tensor([int(r["team_a_idx"]) for r in train_records],
                              dtype=torch.long)
        idx_b = torch.tensor([int(r["team_b_idx"]) for r in train_records],
                              dtype=torch.long)

        fa_proj, fb_proj = pred.project_feats(fa_t, fb_t)
        ea = embeddings[idx_a] + fa_proj
        eb = embeddings[idx_b] + fb_proj

        neutral = torch.tensor(
            [[float(r["neutral"])] for r in train_records],
            dtype=torch.float
        )
        sa_all, sb_all = pred(ea, eb, neutral)

        scores_a = torch.tensor([float(r["score_a"]) for r in train_records])
        scores_b = torch.tensor([float(r["score_b"]) for r in train_records])

        # Score loss only — weighted by recency
        loss_sa = (huber(sa_all, scores_a) * train_weights).sum()
        loss_sb = (huber(sb_all, scores_b) * train_weights).sum()
        total_loss = loss_sa + loss_sb

        total_loss.backward()

        torch.nn.utils.clip_grad_norm_(
            list(gnn.parameters()) + list(pred.parameters()), max_norm=1.0
        )
        optimizer.step()
        scheduler.step()

        if epoch % 50 == 0 or epoch == epochs - 1:
            print(f"  Epoch {epoch:>3d}  "
                  f"Score A loss: {loss_sa.item():.2f}  "
                  f"Score B loss: {loss_sb.item():.2f}  "
                  f"LR: {scheduler.get_last_lr()[0]:.5f}")

    # ── Evaluate ──────────────────────────────────────────────────────────────
    print("\nEvaluating on held-out test set...")
    test_metrics  = evaluate(gnn, pred, test_records,
                              test_fa, test_fb, x, edge_index, edge_attr)
    train_metrics = evaluate(gnn, pred, train_records,
                              train_fa, train_fb, x, edge_index, edge_attr)

    print(f"\n── Test Set Results ({test_metrics['n_games']} games) ──")
    print(f"  Winner accuracy: {test_metrics['accuracy']:.1%}  "
          f"(derived from predicted margin)")
    print(f"  Direction acc:   {test_metrics['direction_acc']:.1%}  "
          f"(correct margin sign)")
    print(f"  MAE home score:  {test_metrics['mae_home']:.2f} pts")
    print(f"  MAE away score:  {test_metrics['mae_away']:.2f} pts")
    print(f"  MAE avg:         {test_metrics['mae_points']:.2f} pts")
    print(f"  RMSE avg:        {test_metrics['rmse_points']:.2f} pts")
    print(f"  MAE margin:      {test_metrics['mae_margin']:.2f} pts")
    print(f"  Brier (implied): {test_metrics['brier_score']:.4f}")

    # ── Save ──────────────────────────────────────────────────────────────────
    torch.save(gnn.state_dict(),  os.path.join(save_dir, "gnn.pt"))
    torch.save(pred.state_dict(), os.path.join(save_dir, "predictor.pt"))
    with open(os.path.join(save_dir, "meta.pkl"), "wb") as f:
        pickle.dump({
            "team_names":    list(team_names),
            "in_channels":   in_channels,
            "feat_dim":      feat_dim,
            "half_life":     half_life_days,
            "trained_date":  datetime.today().isoformat(),
            "test_metrics":  test_metrics,
            "train_metrics": train_metrics,
            "n_train":       len(train_records),
            "n_test":        len(test_records)
        }, f)

    print(f"\nModel saved to: {save_dir}")
    return test_metrics
