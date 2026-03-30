import torch
import numpy as np
import pickle
import os
from gnn_model import TeamGNN, MatchupPredictor

_gnn  = None
_pred = None
_meta = None

def _load_models(save_dir="../data/models"):
    global _gnn, _pred, _meta
    if _gnn is not None:
        return
    with open(os.path.join(save_dir, "meta.pkl"), "rb") as f:
        _meta = pickle.load(f)
    _gnn  = TeamGNN(_meta["in_channels"])
    _pred = MatchupPredictor()
    _gnn.load_state_dict(
        torch.load(os.path.join(save_dir, "gnn.pt"), map_location="cpu"))
    _pred.load_state_dict(
        torch.load(os.path.join(save_dir, "predictor.pt"), map_location="cpu"))
    _gnn.eval()
    _pred.eval()

def predict_matchup(node_features, edge_src, edge_dst, edge_weights,
                    team_a_idx, team_b_idx, save_dir="../data/models"):
    _load_models(save_dir)

    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)

    with torch.no_grad():
        embeddings = _gnn(x, edge_index, edge_attr)
        ea = embeddings[int(team_a_idx)].unsqueeze(0)
        eb = embeddings[int(team_b_idx)].unsqueeze(0)
        wp, sa, sb = _pred(ea, eb)

    return {
        "win_prob_a":   float(wp),
        "win_prob_b":   float(1 - wp),
        "pred_score_a": float(sa),
        "pred_score_b": float(sb),
    }
