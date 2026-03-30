import torch
import torch.nn.functional as F
import numpy as np
import os
import pickle
import pandas as pd
from datetime import datetime
from gnn_model import TeamGNN, MatchupPredictor


def to_records(matchup_labels):
    df = pd.DataFrame(matchup_labels)
    return df.to_dict(orient="records")


def evaluate(gnn, pred, records, game_feats_a, game_feats_b,
             x, edge_index, edge_attr):
    """
    Leak-free evaluation: uses point-in-time feature vectors per game
    rather than current season ratings.
    """
    gnn.eval(); pred.eval()

    with torch.no_grad():
        # Get graph embeddings (structural relationships)
        embeddings = gnn(x, edge_index, edge_attr)

        # Build per-game feature tensors
        fa = torch.tensor(np.array(game_feats_a), dtype=torch.float)
        fb = torch.tensor(np.array(game_feats_b), dtype=torch.float)

        # Blend graph embedding with point-in-time features
        idx_a = torch.tensor([int(r["team_a_idx"]) for r in records],
                              dtype=torch.long)
        idx_b = torch.tensor([int(r["team_b_idx"]) for r in records],
                              dtype=torch.long)

        ea = embeddings[idx_a] + fa  # additive blend
        eb = embeddings[idx_b] + fb

        x_cat   = torch.cat([ea, eb], dim=-1)
        x_cat   = F.relu(pred.fc1(x_cat))
        x_cat   = F.relu(pred.fc2(x_cat))
        pred_wp = torch.sigmoid(pred.win_prob(x_cat)).squeeze().numpy()
        pred_sa = (F.softplus(pred.score_a(x_cat)) + 50).squeeze().numpy()
        pred_sb = (F.softplus(pred.score_b(x_cat)) + 50).squeeze().numpy()

    actuals_wp = np.array([float(r["winner"])  for r in records])
    actuals_sa = np.array([float(r["score_a"]) for r in records])
    actuals_sb = np.array([float(r["score_b"]) for r in records])

    # ── Classification ────────────────────────────────────────────────────────
    accuracy = float(np.mean((pred_wp >= 0.5).astype(int) == actuals_wp))
    brier    = float(np.mean((pred_wp - actuals_wp) ** 2))
    eps      = 1e-7
    log_loss = float(-np.mean(
        actuals_wp * np.log(pred_wp + eps) +
        (1 - actuals_wp) * np.log(1 - pred_wp + eps)
    ))

    # ── Scores ────────────────────────────────────────────────────────────────
    mae  = float((np.mean(np.abs(pred_sa - actuals_sa)) +
                  np.mean(np.abs(pred_sb - actuals_sb))) / 2)
    rmse = float((np.sqrt(np.mean((pred_sa - actuals_sa) ** 2)) +
                  np.sqrt(np.mean((pred_sb - actuals_sb) ** 2))) / 2)

    # ── ROI ───────────────────────────────────────────────────────────────────
    roi_by_threshold = {}
    for threshold in [0.55, 0.60, 0.65]:
        bets_a = pred_wp >= threshold
        bets_b = (1 - pred_wp) >= threshold
        returns = []
        for j in range(len(pred_wp)):
            if bets_a[j]:
                returns.append(100/110 if actuals_wp[j] == 1 else -1.0)
            elif bets_b[j]:
                returns.append(100/110 if actuals_wp[j] == 0 else -1.0)
        n_bets   = len(returns)
        roi      = float(np.mean(returns)) * 100 if n_bets > 0 else 0.0
        win_rate = float(np.mean([r > 0 for r in returns])) if n_bets > 0 else 0.0
        roi_by_threshold[str(threshold)] = {
            "n_bets":   n_bets,
            "win_rate": win_rate,
            "roi_pct":  roi
        }

    return {
        "accuracy":         accuracy,
        "brier_score":      brier,
        "log_loss":         log_loss,
        "mae_points":       mae,
        "rmse_points":      rmse,
        "n_games":          len(records),
        "roi_by_threshold": roi_by_threshold
    }


def train(node_features, edge_src, edge_dst, edge_weights,
          team_names, matchup_labels, game_feats_a, game_feats_b,
          recency_weights, half_life_days=60.0, epochs=300, lr=1e-3,
          test_fraction=0.2, save_dir="../data/models"):

    os.makedirs(save_dir, exist_ok=True)

    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)
    in_channels = x.shape[1]

    records     = to_records(matchup_labels)
    rec_weights = list(recency_weights)

    # Per-game point-in-time feature tensors
    fa_all = np.array(game_feats_a)
    fb_all = np.array(game_feats_b)

    # Align feature dims with node features if needed
    feat_dim = x.shape[1]
    if fa_all.shape[1] != feat_dim:
        # Pad or truncate to match
        def align(arr, dim):
            if arr.shape[1] < dim:
                pad = np.zeros((arr.shape[0], dim - arr.shape[1]))
                return np.hstack([arr, pad])
            return arr[:, :dim]
        fa_all = align(fa_all, feat_dim)
        fb_all = align(fb_all, feat_dim)

    # ── Chronological train/test split ────────────────────────────────────────
    n_total  = len(records)
    n_train  = int(n_total * (1 - test_fraction))

    train_records  = records[:n_train]
    test_records   = records[n_train:]
    train_fa       = fa_all[:n_train]
    train_fb       = fb_all[:n_train]
    test_fa        = fa_all[n_train:]
    test_fb        = fb_all[n_train:]
    train_weights  = torch.tensor(rec_weights[:n_train], dtype=torch.float)

    print(f"Train: {len(train_records)} games | "
          f"Test: {len(test_records)} games | "
          f"half-life={half_life_days}d | epochs={epochs}")
    print("Note: using point-in-time features — no data leakage.")

    # ── Models ────────────────────────────────────────────────────────────────
    gnn  = TeamGNN(in_channels)
    pred = MatchupPredictor(embed_dim=in_channels)
    optimizer = torch.optim.Adam(
        list(gnn.parameters()) + list(pred.parameters()),
        lr=lr, weight_decay=1e-4
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=epochs
    )

    bce = torch.nn.BCELoss(reduction="none")
    mse = torch.nn.MSELoss(reduction="none")

    for epoch in range(epochs):
        gnn.train(); pred.train()
        optimizer.zero_grad()

        embeddings = gnn(x, edge_index, edge_attr)

        # Convert training features to tensors
        fa_t = torch.tensor(train_fa, dtype=torch.float)
        fb_t = torch.tensor(train_fb, dtype=torch.float)

        idx_a = torch.tensor([int(r["team_a_idx"]) for r in train_records],
                              dtype=torch.long)
        idx_b = torch.tensor([int(r["team_b_idx"]) for r in train_records],
                              dtype=torch.long)

        # Blend graph embedding + point-in-time features
        ea = embeddings[idx_a] + fa_t
        eb = embeddings[idx_b] + fb_t

        # Vectorized forward pass over all training games
        x_cat   = torch.cat([ea, eb], dim=-1)
        x_cat_h = F.relu(pred.fc1(x_cat))
        x_cat_h = F.relu(pred.fc2(x_cat_h))
        wp_all  = torch.sigmoid(pred.win_prob(x_cat_h)).squeeze()
        sa_all  = F.softplus(pred.score_a(x_cat_h)).squeeze() + 50
        sb_all  = F.softplus(pred.score_b(x_cat_h)).squeeze() + 50

        winners  = torch.tensor([float(r["winner"])  for r in train_records])
        scores_a = torch.tensor([float(r["score_a"]) for r in train_records])
        scores_b = torch.tensor([float(r["score_b"]) for r in train_records])

        loss_wp = (bce(wp_all, winners)  * train_weights).sum()
        loss_sc = ((mse(sa_all, scores_a) +
                    mse(sb_all, scores_b)) * train_weights * 0.005).sum()

        total_loss = loss_wp + loss_sc
        total_loss.backward()

        torch.nn.utils.clip_grad_norm_(
            list(gnn.parameters()) + list(pred.parameters()), max_norm=1.0
        )
        optimizer.step()
        scheduler.step()

        if epoch % 50 == 0 or epoch == epochs - 1:
            print(f"  Epoch {epoch:>3d}  "
                  f"WP loss: {loss_wp.item():.4f}  "
                  f"Score loss: {loss_sc.item():.4f}  "
                  f"LR: {scheduler.get_last_lr()[0]:.5f}")

    # ── Evaluate ──────────────────────────────────────────────────────────────
    print("\nEvaluating on held-out test set (point-in-time features)...")
    test_metrics  = evaluate(gnn, pred, test_records,
                              test_fa, test_fb, x, edge_index, edge_attr)
    train_metrics = evaluate(gnn, pred, train_records,
                              train_fa, train_fb, x, edge_index, edge_attr)

    print(f"\n── Test Set Results ({test_metrics['n_games']} games) ──")
    print(f"  Accuracy:    {test_metrics['accuracy']:.1%}")
    print(f"  Brier Score: {test_metrics['brier_score']:.4f}")
    print(f"  Log Loss:    {test_metrics['log_loss']:.4f}")
    print(f"  MAE Points:  {test_metrics['mae_points']:.2f} pts")
    print(f"  RMSE Points: {test_metrics['rmse_points']:.2f} pts")
    print(f"\n── ROI Simulation (vs -110 line) ──")
    for thresh, roi in test_metrics["roi_by_threshold"].items():
        print(f"  {float(thresh):.0%} threshold: "
              f"{roi['n_bets']} bets | "
              f"Win rate: {roi['win_rate']:.1%} | "
              f"ROI: {roi['roi_pct']:+.2f}%")

    # ── Save ──────────────────────────────────────────────────────────────────
    torch.save(gnn.state_dict(),  os.path.join(save_dir, "gnn.pt"))
    torch.save(pred.state_dict(), os.path.join(save_dir, "predictor.pt"))
    with open(os.path.join(save_dir, "meta.pkl"), "wb") as f:
        pickle.dump({
            "team_names":    list(team_names),
            "in_channels":   in_channels,
            "half_life":     half_life_days,
            "trained_date":  datetime.today().isoformat(),
            "test_metrics":  test_metrics,
            "train_metrics": train_metrics,
            "n_train":       len(train_records),
            "n_test":        len(test_records)
        }, f)

    print("\nModel saved.")
    return test_metrics
