import torch
import torch.nn.functional as F
from torch_geometric.nn import GATConv

class TeamGNN(torch.nn.Module):
    def __init__(self, in_channels, hidden_channels=64, out_channels=32, heads=4):
        super().__init__()
        self.out_channels = out_channels
        self.gat1 = GATConv(in_channels, hidden_channels, heads=heads, dropout=0.3)
        self.gat2 = GATConv(hidden_channels * heads, out_channels, heads=1, dropout=0.3)

    def forward(self, x, edge_index, edge_attr=None):
        x = F.elu(self.gat1(x, edge_index))
        x = self.gat2(x, edge_index)
        return x  # [num_teams, out_channels]


class MatchupPredictor(torch.nn.Module):
    def __init__(self, embed_dim=32, hidden=64, feat_dim=None):
        super().__init__()
        self.embed_dim = embed_dim

        # Project per-game features to embed_dim if dimensions differ
        self.feat_proj = None
        if feat_dim is not None and feat_dim != embed_dim:
            self.feat_proj = torch.nn.Linear(feat_dim, embed_dim)

        self.fc1      = torch.nn.Linear(embed_dim * 2, hidden)
        self.fc2      = torch.nn.Linear(hidden, hidden // 2)
        self.win_prob = torch.nn.Linear(hidden // 2, 1)
        self.score_a  = torch.nn.Linear(hidden // 2, 1)
        self.score_b  = torch.nn.Linear(hidden // 2, 1)

    def project_feats(self, fa, fb):
        """Project per-game features to embed_dim if needed."""
        if self.feat_proj is not None:
            fa = self.feat_proj(fa)
            fb = self.feat_proj(fb)
        return fa, fb

    def forward(self, emb_a, emb_b):
        x = torch.cat([emb_a, emb_b], dim=-1)
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        return (
            torch.sigmoid(self.win_prob(x)).squeeze(),
            F.softplus(self.score_a(x)).squeeze() + 50,
            F.softplus(self.score_b(x)).squeeze() + 50,
        )
