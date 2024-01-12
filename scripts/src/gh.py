import os
from github import Github
from github.GithubException import GithubException
import pandas as pd
import requests
from io import StringIO


def create_or_update_release(
    df: str,
    file_name: str,
    repo_name: str,
    f_dedup,
    env_var_name: str = "GITHUB_ACCESS_TOKEN",
    tag: str = "v1.0.0",
    description: str = "Description of release",
) -> bool:
    """
    Function to upload a dataframe to a GitHub release. Checks for an existing file and combines the new dataframe with the existing, running a deduplication procedure specified by the user.

    Called for its side effects.
    """

    if not isinstance(df, pd.DataFrame):
        raise Exception("`df` should be a `DataFrame`")

    access_token = os.getenv(env_var_name)
    gh = Github(access_token)
    repo = gh.get_user().get_repo(repo_name)

    release = None
    for r in repo.get_releases():
        if r.tag_name == tag:
            release = r
            break

    if release is None:
        try:
            release = repo.create_git_release(tag=tag, name=tag, message=description)
            print(f"New release created: {release.tag_name}")
        except GithubException as e:
            print(f"Error creating release: {e}")
            return False
    else:
        try:
            existing_data_file = None
            for asset in release.get_assets():
                if asset.name == os.path.basename(file_name):
                    existing_data_file = asset
                    break

            if existing_data_file:
                print("Combining new data with data existing in release.")
                response = requests.get(existing_data_file.browser_download_url)
                existing_data = pd.read_csv(StringIO(response.text), dtype="object")
                combined_data = pd.concat([existing_data, df], ignore_index=True)
                existing_data_file.delete_asset()
            else:
                print("No existing data in release. Uploading new data.")
                combined_data = df

            deduped_data = f_dedup(combined_data)

            csv_file = StringIO()
            deduped_data.to_csv(csv_file, index=False)
            file_content = csv_file.getvalue().encode()

        except GithubException as e:
            print(f"Error updating release: {e}")
            return False

    try:
        asset = release.upload_asset_from_memory(
            file_like=file_content, file_size=len(file_content), name=file_name
        )
        print(f"File uploaded: {asset.name}")
        return True
    except GithubException as e:
        print(f"Error uploading file: {e}")
        return False
