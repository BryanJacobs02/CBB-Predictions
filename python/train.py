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


def evaluate(gnn, pred, records, rec_weights, x, edge_index, edge_attr):
    """
    Run evaluation on a set of records.
    Returns dict of metrics.
    """
    gnn.eval(); pred.eval()
    
    actuals_wp  = []
    pred_wp     = []
    actuals_sa  = []
    actuals_sb  = []
    pred_sa     = []
    pred_sb     = []

    with torch.no_grad():
        embeddings = gnn(x, edge_index, edge_attr)
        for i, row in enumerate(records):
            ea = embeddings[int(row["team_a_idx"])].unsqueeze(0)
            eb = embeddings[int(row["team_b_idx"])].unsqueeze(0)
            wp, sa, sb = pred(ea, eb)

            actuals_wp.append(float(row["winner"]))
            pred_wp.append(float(wp))
            actuals_sa.append(float(row["score_a"]))
            actuals_sb.append(float(row["score_b"]))
            pred_sa.append(float(sa))
            pred_sb.append(float(sb))

    actuals_wp = np.array(actuals_wp)
    pred_wp    = np.array(pred_wp)
    actuals_sa = np.array(actuals_sa)
    actuals_sb = np.array(actuals_sb)
    pred_sa    = np.array(pred_sa)
    pred_sb    = np.array(pred_sb)

    # ── Classification metrics ────────────────────────────────────────────────
    predicted_winner = (pred_wp >= 0.5).astype(int)
    accuracy = float(np.mean(predicted_winner == actuals_wp))

    # Brier score: mean squared error of probabilities
    # For winner=1 (home), we use pred_wp; for winner=0 (away), we use 1-pred_wp
    brier = float(np.mean((pred_wp - actuals_wp) ** 2))

    # Log loss
    eps = 1e-7
    log_loss = float(-np.mean(
        actuals_wp * np.log(pred_wp + eps) +
        (1 - actuals_wp) * np.log(1 - pred_wp + eps)
    ))

    # ── Score metrics ─────────────────────────────────────────────────────────
    mae_a  = float(np.mean(np.abs(pred_sa - actuals_sa)))
    mae_b  = float(np.mean(np.abs(pred_sb - actuals_sb)))
    mae    = (mae_a + mae_b) / 2

    rmse_a = float(np.sqrt(np.mean((pred_sa - actuals_sa) ** 2)))
    rmse_b = float(np.sqrt(np.mean((pred_sb - actuals_sb) ** 2)))
    rmse   = (rmse_a + rmse_b) / 2

    # ── ROI simulation at multiple thresholds ─────────────────────────────────
    # Assumes standard -110 line: bet 110 to win 100
    # Payout multiplier: win = +100/110 = 0.909, loss = -1.0
    roi_by_threshold = {}
    for threshold in [0.55, 0.60, 0.65]:
        # Bet on team_a when pred_wp >= threshold
        # Bet on team_b when 1-pred_wp >= threshold
        bets_a   = pred_wp >= threshold
        bets_b   = (1 - pred_wp) >= threshold
        
        returns = []
        for j in range(len(pred_wp)):
            if bets_a[j]:
                won = actuals_wp[j] == 1
                returns.append(100/110 if won else -1.0)
            elif bets_b[j]:
                won = actuals_wp[j] == 0
                returns.append(100/110 if won else -1.0)
        
        n_bets = len(returns)
        roi    = float(np.mean(returns)) * 100 if n_bets > 0 else 0.0
        win_rate = float(np.mean([r > 0 for r in returns])) if n_bets > 0 else 0.0
        roi_by_threshold[str(threshold)] = {
            "n_bets":   n_bets,
            "win_rate": win_rate,
            "roi_pct":  roi
        }

    return {
        "accuracy":          accuracy,
        "brier_score":       brier,
        "log_loss":          log_loss,
        "mae_points":        mae,
        "rmse_points":       rmse,
        "n_games":           len(records),
        "roi_by_threshold":  roi_by_threshold
    }


def train(node_features, edge_src, edge_dst, edge_weights,
          team_names, matchup_labels, recency_weights,
          half_life_days=60.0, epochs=300, lr=1e-3,
          test_fraction=0.2, save_dir="../data/models"):

    os.makedirs(save_dir, exist_ok=True)

    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)
    in_channels = x.shape[1]

    records     = to_records(matchup_labels)
    rec_weights = list(recency_weights)

    # ── Chronological train/test split ────────────────────────────────────────
    # Records are already sorted by date from R
    n_total     = len(records)
    n_train     = int(n_total * (1 - test_fraction))
    
    train_records = records[:n_train]
    test_records  = records[n_train:]
    train_weights = torch.tensor(rec_weights[:n_train], dtype=torch.float)

    print(f"Train: {len(train_records)} games | "
          f"Test: {len(test_records)} games | "
          f"half-life={half_life_days}d | epochs={epochs}")

    # ── Models ────────────────────────────────────────────────────────────────
    gnn  = TeamGNN(in_channels)
    pred = MatchupPredictor()
    optimizer = torch.optim.Adam(
        list(gnn.parameters()) + list(pred.parameters()),
        lr=lr, weight_decay=1e-4
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs)

    bce = torch.nn.BCELoss(reduction="none")
    mse = torch.nn.MSELoss(reduction="none")

    for epoch in range(epochs):
        gnn.train(); pred.train()
        optimizer.zero_grad()

        embeddings = gnn(x, edge_index, edge_attr)

        loss_wp_total = torch.tensor(0.0)
        loss_sc_total = torch.tensor(0.0)

        for i, row in enumerate(train_records):
            ea = embeddings[int(row["team_a_idx"])].unsqueeze(0)
            eb = embeddings[int(row["team_b_idx"])].unsqueeze(0)
            wp, sa, sb = pred(ea, eb)

            w = train_weights[i]

            lw = bce(wp, torch.tensor(float(row["winner"]))) * w
            ls = (mse(sa, torch.tensor(float(row["score_a"]))) +
                  mse(sb, torch.tensor(float(row["score_b"])))) * w * 0.005

            loss_wp_total = loss_wp_total + lw
            loss_sc_total = loss_sc_total + ls

        total_loss = loss_wp_total + loss_sc_total
        total_loss.backward()

        torch.nn.utils.clip_grad_norm_(
            list(gnn.parameters()) + list(pred.parameters()), max_norm=1.0
        )

        optimizer.step()
        scheduler.step()

        if epoch % 50 == 0 or epoch == epochs - 1:
            print(f"  Epoch {epoch:>3d}  "
                  f"WP loss: {loss_wp_total.item():.4f}  "
                  f"Score loss: {loss_sc_total.item():.4f}  "
                  f"LR: {scheduler.get_last_lr()[0]:.5f}")

    # ── Evaluate on held-out test set ─────────────────────────────────────────
    print("\nEvaluating on test set...")
    test_metrics  = evaluate(gnn, pred, test_records,  None, x, edge_index, edge_attr)
    train_metrics = evaluate(gnn, pred, train_records, None, x, edge_index, edge_attr)

    print(f"\n── Test Set Results ({test_metrics['n_games']} games) ──")
    print(f"  Accuracy:    {test_metrics['accuracy']:.1%}")
    print(f"  Brier Score: {test_metrics['brier_score']:.4f}  (lower is better)")
    print(f"  Log Loss:    {test_metrics['log_loss']:.4f}  (lower is better)")
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
