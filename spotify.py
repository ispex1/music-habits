import sys
import pandas as pd 
import configparser
import spotipy
from spotipy.oauth2 import SpotifyOAuth

# SPOTIFY API  #

config = configparser.ConfigParser()
config.read('spotify_credentials.txt')

client_id = config.get('Credentials', 'CLIENT_ID')
client_secret = config.get('Credentials', 'CLIENT_SECRET')
redirect_uri = config.get('Credentials', 'REDIRECT_URI')

sp = spotipy.Spotify(auth_manager=SpotifyOAuth(client_id=client_id, client_secret=client_secret, redirect_uri=redirect_uri, scope='user-library-read'))

print('Spotify API connection established')

def get_artist_genres(artist_name):
    results = sp.search(q=artist_name, type='artist')
    if results['artists']['items']:
        artist = results['artists']['items'][0]
        genres = artist['genres']
        return genres
    else:
        return None

# ------------- TEST --------------- # 
'''
dtest = pd.read_json('data/endsong_0.json', orient='records')

# Delete rows with missing artist name
dtest.dropna(subset=['master_metadata_album_artist_name'], inplace=True)

#print uniques artists
artists = dtest['master_metadata_album_artist_name'].unique().tolist()

with open('output.txt', 'w', encoding='utf-8') as f:
    f.write(f"Unique artists: {len(artists)}\n")
    # Create a dictionary with artist names as keys and genres as values
    artists_genres = {}
    for artist in artists:
        f.write(f"{artists.index(artist)} - {artist}\n")
        artists_genres[artist] = get_artist_genres(artist)
        
# add genre column to dtest
dtest['genres'] = dtest['master_metadata_album_artist_name'].map(artists_genres)

# Save DataFrame to CSV file
csv_file_path = 'data/test.csv'
dtest.to_csv(csv_file_path, index=False)
print(f"Test saved to CSV file: {csv_file_path}")
'''
# ---------------------------------

# Read iteratively endsong_n.json files and save them to a single DataFrame
df = pd.DataFrame()
for i in range(8):
    df = pd.concat([df, pd.read_json(f'data/endsong_{i}.json', orient='records')])

# Delete rows with missing artist name
df.dropna(subset=['master_metadata_album_artist_name'], inplace=True)

# Print total number of artists
total_artists = df['master_metadata_album_artist_name'].unique().tolist()
print(f"Total artists: {len(total_artists)}")

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

#print uniques artists
artists = df['master_metadata_album_artist_name'].unique().tolist()

with open('output.txt', 'w', encoding='utf-8') as f:
    f.write(f"Unique artists: {len(artists)}\n")
    # Create a dictionary with artist names as keys and genres as values
    artists_genres = {}
    for artist in artists:
        f.write(f"{artists.index(artist)} - {artist}\n")
        artists_genres[artist] = get_artist_genres(artist)
        
# add genre column to dtest
df['genres'] = df['master_metadata_album_artist_name'].map(artists_genres)

# Save DataFrame to CSV file
csv_file_path = 'data/data.csv'
df.to_csv(csv_file_path, index=False)
print(f"DataFrame saved to CSV file: {csv_file_path}")
