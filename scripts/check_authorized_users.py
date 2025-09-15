"""
Checks if a user is authorized to modify model directories
in a Hub.
"""

import argparse
import json
import sys
from pathlib import Path


def get_authorized_dirs_and_users(authorized_users_path: Path) -> dict:
    """
    Load authorized users from a JSON file.

    Parameters
    ----------
    authorized_users_path : Path
        Path to the JSON file containing authorized users.

    Returns
    -------
    dict
        Dictionary mapping model IDs to lists of
        authorized users.
    """
    if authorized_users_path.suffix.lower() != ".json":
        return {}
    try:
        with open(authorized_users_path, "r") as f:
            auth_data = json.load(f)
        return {
            entry["model_id"]: entry["authorized_github_users"]
            for entry in auth_data
        }
    except Exception as e:
        print(f"Error reading JSON: {e}")
        return {}


def main(
    changed_dirs: list[str],
    gh_actor: str,
    authorized_users_path: str | Path,
) -> None:
    """
    Check if a user is authorized to modify specified
    directories.

    Parameters
    ----------
    changed_dirs : list[str]
        List of directory names that have been changed.
    gh_actor : str
        GitHub username of the person making changes.
    authorized_users_path : str | Path
        Path to the JSON file containing authorized users.

    Returns
    -------
    None
    """
    dir_users_map = get_authorized_dirs_and_users(Path(authorized_users_path))

    if not dir_users_map:
        print(f"Error: could not load user data from {authorized_users_path}")
        sys.exit(1)

    for dir in changed_dirs:
        if dir not in dir_users_map:
            print(f"Error: {dir} is not authorized for modification.")
            sys.exit(1)

        user_list = dir_users_map[dir]

        if not user_list:
            print(
                f"Error: Changes found in '{dir}/', but no authorized users listed."
            )
            sys.exit(1)

        if gh_actor not in user_list:
            print(
                f"Error: Only the following users can modify '{dir}/': {user_list}"
            )
            sys.exit(1)

    print(f"Success: changes authorized for user '{gh_actor}'.")
    Path("status").write_text("success")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Check if user is authorized to modify directories in model-output/"
    )
    parser.add_argument(
        "changed_dirs",
        type=str,
        help="Space-separated list of changed directories.",
    )
    parser.add_argument(
        "gh_actor",
        type=str,
        help="GitHub actor (username) making the changes.",
    )
    parser.add_argument(
        "authorized_users_path",
        type=Path,
        help="Path to the file containing authorized users.",
    )
    args = parser.parse_args()
    args.changed_dirs = args.changed_dirs.split()
    main(**vars(args))
