import torch
import torch.nn.functional as F
import numpy as np
import os
import pickle
from datetime import datetime
from gnn_model import TeamGNN, MatchupPredictor


def compute_recency_weights(game_dates: list[str],
                             half_life_days: float = 60.0) -> torch.Tensor:
    """
    Exponential decay: games played half_life_days ago have 50% the weight
    of a game played today. This makes the model prioritize recent form
    without completely discarding older seasons.

    half_life_days=60 means:
      - Game today          → weight 1.00
      - Game 60 days ago    → weight 0.50
      - Game 120 days ago   → weight 0.25
      - Game from 2 yrs ago → weight ~0.02
    """
    today = datetime.today()
    weights = []
    for d in game_dates:
        days_ago = (today - datetime.strptime(d, "%Y-%m-%d")).days
        w = 0.5 ** (days_ago / half_life_days)
        weights.append(max(w, 1e-4))  # floor to avoid zero weight
    return torch.tensor(weights, dtype=torch.float)


def train(node_features, edge_src, edge_dst, edge_weights,
          team_names, matchup_labels,
          epochs=300, lr=1e-3, half_life_days=60.0,
          save_dir="../data/models"):
    """
    Parameters
    ----------
    node_features   : 2D array [num_teams x num_features]
    edge_src/dst    : 1D arrays of edge indices (0-indexed)
    edge_weights    : 1D array of edge weights
    team_names      : list of team name strings
    matchup_labels  : list of dicts with keys:
                        team_a_idx, team_b_idx, winner,
                        score_a, score_b, game_date
    half_life_days  : recency decay — lower = more weight on recent games
    """
    os.makedirs(save_dir, exist_ok=True)

    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)
    in_channels = x.shape[1]

    # ── Recency weights ───────────────────────────────────────────────────────
    game_dates = [str(row["game_date"]) for row in matchup_labels]
    rec_weights = compute_recency_weights(game_dates, half_life_days)

    # ── Models ────────────────────────────────────────────────────────────────
    gnn  = TeamGNN(in_channels)
    pred = MatchupPredictor()
    optimizer = torch.optim.Adam(
        list(gnn.parameters()) + list(pred.parameters()), lr=lr,
        weight_decay=1e-4
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs)

    bce = torch.nn.BCELoss(reduction="none")  # keep per-sample for weighting
    mse = torch.nn.MSELoss(reduction="none")

    print(f"Training on {len(matchup_labels)} games | "
          f"half-life={half_life_days}d | epochs={epochs}")

    for epoch in range(epochs):
        gnn.train(); pred.train()
        optimizer.zero_grad()

        embeddings = gnn(x, edge_index, edge_attr)

        loss_wp_total = torch.tensor(0.0)
        loss_sc_total = torch.tensor(0.0)

        for i, row in enumerate(matchup_labels):
            ea = embeddings[int(row["team_a_idx"])].unsqueeze(0)
            eb = embeddings[int(row["team_b_idx"])].unsqueeze(0)
            wp, sa, sb = pred(ea, eb)

            w = rec_weights[i]

            # Win probability loss (weighted BCE)
            lw = bce(wp, torch.tensor(float(row["winner"]))) * w
            # Score loss (weighted MSE, scaled down so it doesn't overpower win prob)
            ls = (mse(sa, torch.tensor(float(row["score_a"]))) +
                  mse(sb, torch.tensor(float(row["score_b"])))) * w * 0.005

            loss_wp_total = loss_wp_total + lw
            loss_sc_total = loss_sc_total + ls

        total_loss = loss_wp_total + loss_sc_total
        total_loss.backward()

        # Gradient clipping for stability
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

    # ── Save ──────────────────────────────────────────────────────────────────
    torch.save(gnn.state_dict(),  os.path.join(save_dir, "gnn.pt"))
    torch.save(pred.state_dict(), os.path.join(save_dir, "predictor.pt"))
    with open(os.path.join(save_dir, "meta.pkl"), "wb") as f:
        pickle.dump({
            "team_names":   list(team_names),
            "in_channels":  in_channels,
            "half_life":    half_life_days,
            "trained_date": datetime.today().isoformat()
        }, f)

    print("Model saved.")
    return True
