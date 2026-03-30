import torch
import torch.nn.functional as F
from torch_geometric.nn import GATConv

class TeamGNN(torch.nn.Module):
    """
    Graph Attention Network producing per-team embeddings.
    GAT attention weights let the model learn which inter-team
    relationships (conference rivals, common opponents) matter most.
    """
    def __init__(self, in_channels, hidden_channels=64, out_channels=32, heads=4):
        super().__init__()
        self.gat1 = GATConv(in_channels, hidden_channels, heads=heads, dropout=0.3)
        self.gat2 = GATConv(hidden_channels * heads, out_channels, heads=1, dropout=0.3)

    def forward(self, x, edge_index, edge_attr=None):
        x = F.elu(self.gat1(x, edge_index))
        x = self.gat2(x, edge_index)
        return x  # [num_teams, out_channels]


class MatchupPredictor(torch.nn.Module):
    """
    Takes two team embeddings, outputs:
    - win probability for team A
    - predicted score for team A
    - predicted score for team B
    """
    def __init__(self, embed_dim=32, hidden=64):
        super().__init__()
        self.fc1      = torch.nn.Linear(embed_dim * 2, hidden)
        self.fc2      = torch.nn.Linear(hidden, hidden // 2)
        self.win_prob = torch.nn.Linear(hidden // 2, 1)
        self.score_a  = torch.nn.Linear(hidden // 2, 1)
        self.score_b  = torch.nn.Linear(hidden // 2, 1)

    def forward(self, emb_a, emb_b):
        x = torch.cat([emb_a, emb_b], dim=-1)
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        return (
            torch.sigmoid(self.win_prob(x)).squeeze(),
            F.softplus(self.score_a(x)).squeeze() + 50,
            F.softplus(self.score_b(x)).squeeze() + 50,
        )
