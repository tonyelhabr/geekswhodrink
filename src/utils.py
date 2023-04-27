from gh import create_or_update_release

REPO_NAME = 'geekswhodrink'
RELEASE_TAG = 'data'
RELEASE_DESCRIPTION = 'Data release'
GITHUB_ACCESS_TOKEN_ENV_VAR_NAME = 'GEEKS_WHO_DRINK_TOKEN'

def filter_latest_venues(df):
  return (
      df.sort_values('updated_at', ascending=False).drop_duplicates('venue_id').sort_values('venue_id')
  )

def create_or_update_geekswhodrink_release(
  df, 
  file_name,
  repo_name=REPO_NAME,
  env_var_name=GITHUB_ACCESS_TOKEN_ENV_VAR_NAME, 
  tag=RELEASE_TAG,
  description=RELEASE_DESCRIPTION
):
  create_or_update_release(
    df=df,
    file_name=file_name,
    repo_name=repo_name,
    f_dedup=filter_latest_venues,
    env_var_name=env_var_name,
    tag=tag,
    description=description
  )
