import pandas as pd 

# Read iteratively endsong_n.json files and save them to a single DataFrame
df = pd.DataFrame()
for i in range(8):
    df = pd.concat([df, pd.read_json(f'data/endsong_{i}.json', orient='records')])

# Delete columns that are not useful
df.drop(columns=['username','ip_addr_decrypted', 'user_agent_decrypted', 'episode_name', 'episode_show_name', 'spotify_episode_uri', 'skipped','offline_timestamp', 'incognito_mode'], inplace=True)

# Remove the T and the Z in the timestamp column
df['ts'] = df['ts'].str.replace('T', ' ').str.replace('Z', '')

# Rename platform names for better readability
df.loc[df['platform'].str.contains('tv'),'platform'] = 'TV'
df.loc[df['platform'].str.contains('Android'),'platform'] = 'Smartphone'
df.loc[df['platform'].str.contains('android'),'platform'] = 'Smartphone'
df.loc[df['platform'].str.contains('Windows'),'platform'] = 'Computer'
df.loc[df['platform'].str.contains('windows'),'platform'] = 'Computer'
df.loc[df['platform'].str.contains('Linux'),'platform'] = 'Computer'
df.loc[df['platform'].str.contains('Partner'),'platform'] = 'Home Speaker'

# Save DataFrame to CSV file
csv_file_path = 'data.csv'
df.to_csv(csv_file_path, index=False)
print(f"DataFrame saved to CSV file: {csv_file_path}")