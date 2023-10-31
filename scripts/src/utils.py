from src.gh import create_or_update_release
import pandas as pd

REPO_NAME = "geekswhodrink"
RELEASE_TAG = "data"
RELEASE_DESCRIPTION = "Data release"
GITHUB_ACCESS_TOKEN_ENV_VAR_NAME = "GEEKS_WHO_DRINK_TOKEN"


def filter_latest_venues(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    return (
        df.sort_values("updated_at", ascending=False)
        .drop_duplicates("venue_id")
        .sort_values("venue_id")
    )


def create_or_update_geekswhodrink_release(
    df: pd.DataFrame,
    file_name: str,
    repo_name: str = REPO_NAME,
    env_var_name: str = GITHUB_ACCESS_TOKEN_ENV_VAR_NAME,
    tag: str = RELEASE_TAG,
    description: str = RELEASE_DESCRIPTION,
):
    """
    Function to upload a data frame to the GeeksWhoDrink GitHub repo.
    """
    create_or_update_release(
        df=df,
        file_name=file_name,
        repo_name=repo_name,
        f_dedup=filter_latest_venues,
        env_var_name=env_var_name,
        tag=tag,
        description=description,
    )
