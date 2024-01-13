from src.gh import create_or_update_release
import pandas as pd
import datetime

REPO_NAME = "geekswhodrink"
RELEASE_TAG = "data"
RELEASE_DESCRIPTION = "Data release"
GITHUB_ACCESS_TOKEN_ENV_VAR_NAME = "GEEKS_WHO_DRINK_TOKEN"


def combine_venue_dfs(existing_df: pd.DataFrame, new_df: pd.DataFrame) -> pd.DataFrame:
    current_timestamp = datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")

    if existing_df.empty:
        new_df["created_at"] = current_timestamp
        new_df["updated_at"] = current_timestamp
        return new_df

    existing_df.loc[
        existing_df["venue_id"].isin(new_df["venue_id"]), "updated_at"
    ] = current_timestamp
    existing_df.loc[
        ~existing_df["venue_id"].isin(new_df["venue_id"]), "updated_at"
    ] = None

    new_records = new_df[~new_df["venue_id"].isin(existing_df["venue_id"])]
    new_records["created_at"] = current_timestamp
    new_records["updated_at"] = current_timestamp
    updated_df = pd.concat([existing_df, new_records], ignore_index=True)
    return updated_df.drop_duplicates("venue_id").sort_values("venue_id")


def create_or_update_geekswhodrink_release(
    df: pd.DataFrame,
    file_name: str,
    repo_name: str = REPO_NAME,
    env_var_name: str = GITHUB_ACCESS_TOKEN_ENV_VAR_NAME,
    tag: str = RELEASE_TAG,
    description: str = RELEASE_DESCRIPTION,
) -> bool:
    """
    Function to upload a data frame to the GeeksWhoDrink GitHub repo.
    """
    create_or_update_release(
        df=df,
        file_name=file_name,
        repo_name=repo_name,
        f_update=combine_venue_dfs,
        env_var_name=env_var_name,
        tag=tag,
        description=description,
    )
