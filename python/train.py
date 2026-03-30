import torch
import torch.nn.functional as F
import numpy as np
import os
import pickle
import pandas as pd
from datetime import datetime
from gnn_model import TeamGNN, MatchupPredictor


def compute_recency_weights(game_dates, half_life_days=60.0):
    today = datetime.today()
    weights = []
    for d in game_dates:
        days_ago = (today - datetime.strptime(str(d), "%Y-%m-%d")).days
        w = 0.5 ** (days_ago / half_life_days)
        weights.append(max(w, 1e-4))
    return torch.tensor(weights, dtype=torch.float)


def to_records(matchup_labels):
    """
    Robustly convert whatever reticulate sends into a list of row dicts.
    reticulate can send R lists in several formats depending on structure.
    Using pandas as the universal adapter handles all of them.
    """
    df = pd.DataFrame(matchup_labels)
    return df.to_dict(orient="records")


def train(node_features, edge_src, edge_dst, edge_weights,
          team_names, matchup_labels,
          epochs=300, lr=1e-3, half_life_days=60.0,
          save_dir="../data/models"):

    os.makedirs(save_dir, exist_ok=True)

    # ── Normalize input data ──────────────────────────────────────────────────
    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)
    in_channels = x.shape[1]

    # ── Convert matchup_labels to list of row dicts ───────────────────────────
    records = to_records(matchup_labels)

    # ── Recency weights ───────────────────────────────────────────────────────
    game_dates  = [str(row["game_date"]) for row in records]
    rec_weights = compute_recency_weights(game_dates, half_life_days)

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

    print(f"Training on {len(records)} games | "
          f"half-life={half_life_days}d | epochs={epochs}")

    for epoch in range(epochs):
        gnn.train(); pred.train()
        optimizer.zero_grad()

        embeddings = gnn(x, edge_index, edge_attr)

        loss_wp_total = torch.tensor(0.0)
        loss_sc_total = torch.tensor(0.0)

        for i, row in enumerate(records):
            ea = embeddings[int(row["team_a_idx"])].unsqueeze(0)
            eb = embeddings[int(row["team_b_idx"])].unsqueeze(0)
            wp, sa, sb = pred(ea, eb)

            w = rec_weights[i]

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
