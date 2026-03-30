import torch
import numpy as np
import pickle
import os
from gnn_model import TeamGNN, MatchupPredictor

_DEFAULT_SAVE_DIR = os.path.normpath(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data", "models")
)

_gnn  = None
_pred = None
_meta = None

def _load_models(save_dir=None):
    global _gnn, _pred, _meta
    if save_dir is None:
        save_dir = _DEFAULT_SAVE_DIR
    if _gnn is not None:
        return
    with open(os.path.join(save_dir, "meta.pkl"), "rb") as f:
        _meta = pickle.load(f)
    _gnn  = TeamGNN(_meta["in_channels"])
    _pred = MatchupPredictor(embed_dim=32, hidden=64,
                              feat_dim=_meta.get("feat_dim", 32))
    _gnn.load_state_dict(
        torch.load(os.path.join(save_dir, "gnn.pt"), map_location="cpu"))
    _pred.load_state_dict(
        torch.load(os.path.join(save_dir, "predictor.pt"), map_location="cpu"))
    _gnn.eval()
    _pred.eval()

def predict_matchup(node_features, edge_src, edge_dst, edge_weights,
                    team_a_idx, team_b_idx, save_dir=None):
    _load_models(save_dir)

    x          = torch.tensor(np.array(node_features), dtype=torch.float)
    edge_index = torch.tensor([list(edge_src), list(edge_dst)], dtype=torch.long)
    edge_attr  = torch.tensor(list(edge_weights), dtype=torch.float)

    # Safely extract scalar index from whatever reticulate sends
    def to_int(val):
        if isinstance(val, (list, tuple)):
            return int(val[0])
        return int(val)

    idx_a = to_int(team_a_idx)
    idx_b = to_int(team_b_idx)

    with torch.no_grad():
        embeddings = _gnn(x, edge_index, edge_attr)
        ea = embeddings[idx_a].unsqueeze(0)
        eb = embeddings[idx_b].unsqueeze(0)
        wp, sa, sb = _pred(ea, eb)

    return {
        "win_prob_a":   float(wp),
        "win_prob_b":   float(1 - wp),
        "pred_score_a": float(sa),
        "pred_score_b": float(sb),
    }
