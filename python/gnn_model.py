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
    Explicitly asymmetric architecture:
    - Separate encoding paths for home and away teams
    - Home advantage learned through separate bias terms
    - score_a always predicts HOME score
    - score_b always predicts AWAY score
    """
    def __init__(self, embed_dim=32, hidden=128, feat_dim=None):
        super().__init__()
        self.embed_dim = embed_dim
        actual_feat_dim = feat_dim if feat_dim is not None else embed_dim

        # Separate projections for home and away
        self.home_proj = torch.nn.Linear(actual_feat_dim, embed_dim)
        self.away_proj = torch.nn.Linear(actual_feat_dim, embed_dim)

        # Home and away get separate encoding paths before combining
        self.home_enc = torch.nn.Linear(embed_dim * 2, hidden // 2)
        self.away_enc = torch.nn.Linear(embed_dim * 2, hidden // 2)

        # Combined path + neutral flag
        self.fc_combined = torch.nn.Linear(hidden + 1, hidden // 2)
        self.dropout      = torch.nn.Dropout(0.3)

        # Separate output heads
        self.score_home = torch.nn.Linear(hidden // 2, 1)
        self.score_away = torch.nn.Linear(hidden // 2, 1)

        # Explicit home advantage bias
        self.home_advantage = torch.nn.Parameter(torch.tensor(5.0))

        # Initialize
        for layer in [self.home_proj, self.away_proj,
                       self.home_enc, self.away_enc,
                       self.fc_combined, self.score_home, self.score_away]:
            torch.nn.init.xavier_uniform_(layer.weight, gain=1.4)

    def project_feats(self, fa, fb):
        return self.home_proj(fa), self.away_proj(fb)

    def forward(self, graph_home, graph_away, feat_home, feat_away, neutral=None):
        if neutral is None:
            neutral = torch.zeros(graph_home.shape[0], 1)

        # Separate encoding
        home_rep = F.relu(self.home_enc(torch.cat([graph_home, feat_home], dim=-1)))
        away_rep = F.relu(self.away_enc(torch.cat([graph_away, feat_away], dim=-1)))

        # Combine
        combined = torch.cat([home_rep, away_rep, neutral], dim=-1)
        x = F.relu(self.fc_combined(combined))
        x = self.dropout(x)

        # Predict scores — home advantage is an explicit learned parameter
        # reduced when neutral=1
        home_adv = self.home_advantage * (1 - neutral)
        sa = F.softplus(self.score_home(x)).squeeze() + 50 + home_adv.squeeze()
        sb = F.softplus(self.score_away(x)).squeeze() + 50

        return sa, sb
