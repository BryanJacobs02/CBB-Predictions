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
        return x


class MatchupPredictor(torch.nn.Module):
    """
    Predicts home score and away score.
    Input: concatenated home/away embeddings + neutral site flag.
    """
    def __init__(self, embed_dim=32, hidden=64, feat_dim=None):
        super().__init__()
        self.embed_dim = embed_dim

        actual_feat_dim = feat_dim if feat_dim is not None else embed_dim
        self.feat_proj  = torch.nn.Linear(actual_feat_dim, embed_dim)

        # +1 for neutral site flag
        self.fc1     = torch.nn.Linear(embed_dim * 2 + 1, hidden)
        self.dropout = torch.nn.Dropout(0.3)
        self.fc2     = torch.nn.Linear(hidden, hidden // 2)
        self.score_a = torch.nn.Linear(hidden // 2, 1)  # home score
        self.score_b = torch.nn.Linear(hidden // 2, 1)  # away score

    def project_feats(self, fa, fb):
        return self.feat_proj(fa), self.feat_proj(fb)

    def forward(self, emb_a, emb_b, neutral=None):
        if neutral is None:
            neutral = torch.zeros(emb_a.shape[0], 1)
        x  = torch.cat([emb_a, emb_b, neutral], dim=-1)
        x  = F.relu(self.fc1(x))
        x  = self.dropout(x)
        x  = F.relu(self.fc2(x))
        sa = F.softplus(self.score_a(x)).squeeze() + 50
        sb = F.softplus(self.score_b(x)).squeeze() + 50
        return sa, sb
