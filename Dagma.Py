import pandas as pd
from dagma.linear import DagmaLinear

# Convert the R data frames to pandas DataFrames
data = pd.DataFrame(bootstrap_sample_py)
blacklist = pd.DataFrame(Black_List_py)
whitelist = pd.DataFrame(white_List_py)

# Create a mapping from column names to indices
names_idx = {col: idx for idx, col in enumerate(data.columns)}

# Initialize lists for excluded and included edges
edges_ex = []
edges_in = []

# Populate the list of excluded edges from the blacklist
n_blacklist, _ = blacklist.shape
for i in range(n_blacklist):
  row = blacklist.values[i].tolist()
  edges_ex.append((names_idx[row[0]], names_idx[row[1]]))

# Populate the list of included edges from the whitelist
n_whitelist, _ = whitelist.shape
for i in range(n_whitelist):
  row = whitelist.values[i].tolist()
  edges_in.append((names_idx[row[0]], names_idx[row[1]]))

# Fit the model with both excluded and included edges
model = DagmaLinear(loss_type='l2')
W_est = model.fit(data.values, lambda1=0.1, exclude_edges=edges_ex, include_edges=edges_in)
