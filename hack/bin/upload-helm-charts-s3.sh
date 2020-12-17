#!/usr/bin/env bash

# Upload helm charts from .local/charts/ to mirror on S3.

# The contents of /charts are thus made available under
# https://s3-eu-west-1.amazonaws.com/public.wire.com/charts
# To use the charts:
# helm repo add wire https://s3-eu-west-1.amazonaws.com/public.wire.com/charts
# helm search wire

# This script uses the helm s3 plugin,
# for more info see https://github.com/hypnoglow/helm-s3

set -eo pipefail
set -x

USAGE="Upload helm charts to S3. Usage: $0 to upload all charts or $0 <chart-directory> to sync only a single one. --force-push can be used to override S3 artifacts. --reindex can be used to force a complete reindexing in case the index is malformed."

branch=$(git rev-parse --abbrev-ref HEAD)
if [ $branch == "master" ]; then
    PUBLIC_DIR="charts"
    REPO_NAME="wire"
elif [ $branch == "develop" ]; then
    PUBLIC_DIR="charts-develop"
    REPO_NAME="wire-develop"
else
    echo "You are not on master or develop. Synchronizing charts on a custom branch will push them to the charts-custom helm repository in order not to interfere with versioning on master/develop."
    read -p "Are you sure you want to push to charts-custom? [yN] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        exit 1
    fi
    PUBLIC_DIR="charts-custom"
    REPO_NAME="wire-custom"
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL_DIR=$SCRIPT_DIR/../..
CHART_DIR=$TOP_LEVEL_DIR/.local/charts
cd "$TOP_LEVEL_DIR"

chart_dir=$1

# If ./upload-helm-charts-s3.sh is run with a parameter, only synchronize one chart
if [ -n "$chart_dir" ] && [ -d "$chart_dir" ]; then
    chart_name=$(basename $chart_dir)
    echo "only syncing $chart_name"
    charts=( "$chart_name" )
else
    charts=( $(find $CHART_DIR/ -maxdepth 1 -type d | sed -n "s=$CHART_DIR/\(.\+\)=\1 =p") )
fi

# install s3 plugin if not present
s3_plugin_version=$(helm plugin list | grep "^s3 " | awk '{print $2}' || true)
if [[ $s3_plugin_version != "0.10.0" ]]; then
    echo "not version 0.10.0, upgrading or installing plugin..."
    helm plugin remove s3 || true
    helm plugin install https://github.com/hypnoglow/helm-s3.git --version v0.10.0
fi

# index/sync charts to S3
export AWS_REGION=eu-west-1

# PUBLIC_DIR is set to 'charts' for master or 'charts-develop' for develop above.
S3_URL="s3://public.wire.com/$PUBLIC_DIR"
PUBLIC_URL="https://s3-eu-west-1.amazonaws.com/public.wire.com/$PUBLIC_DIR"

# initialize index file only if file doesn't yet exist
if ! aws s3api head-object --bucket public.wire.com --key "$PUBLIC_DIR/index.yaml" &> /dev/null ; then
    echo "initializing fresh index.yaml"
    helm s3 init "$S3_URL" --publish "$PUBLIC_URL"
fi

helm repo add "$PUBLIC_DIR" "$S3_URL"
helm repo add "$REPO_NAME" "$PUBLIC_URL"

rm ./*.tgz &> /dev/null || true # clean any packaged files, if any
for chart in "${charts[@]}"; do
    echo "Syncing chart $chart..."
    "$SCRIPT_DIR/update.sh" "$CHART_DIR/$chart"
    helm package "$CHART_DIR/${chart}" && sync
    tgz=$(ls "${chart}"-*.tgz)
    echo "syncing ${tgz}..."
    # Push the artifact only if it doesn't already exist
    if ! aws s3api head-object --bucket public.wire.com --key "$PUBLIC_DIR/${tgz}" &> /dev/null ; then
        helm s3 push --relative "$tgz" "$PUBLIC_DIR"
        printf "\n--> pushed %s to S3\n\n" "$tgz"
    else
        if [[ $1 == *--force-push* || $2 == *--force-push* || $3 == *--force-push* ]]; then
            helm s3 push --relative "$tgz" "$PUBLIC_DIR" --force
            printf "\n--> (!) force pushed %s to S3\n\n" "$tgz"
        else
            printf "\n--> %s not changed or not version bumped; doing nothing.\n\n" "$chart"
        fi
    fi
    rm "$tgz"

done

if [[ $1 == *--reindex* || $2 == *--reindex* || $3 == *--reindex* ]]; then
    printf "\n--> (!) Reindexing, this can take a few minutes...\n\n"
    helm s3 reindex --relative "$PUBLIC_DIR"
    # update local cache with newly pushed charts
    helm repo update
    # see all results
    helm search repo "$REPO_NAME/" -l
else
    # update local cache with newly pushed charts
    helm repo update
    printf "\n--> Not reindexing by default. Pass the --reindex flag in case the index.yaml is incomplete. See all wire charts using \n helm search $REPO_NAME/ -l\n\n"
fi


# TODO: improve the above script by exiting with an error if helm charts have changed but a version was not bumped.
# TODO: hash comparison won't work directly: helm package ... results in new md5 hashes each time, even if files don't change. This is due to files being ordered differently in the tar file. See
# * https://github.com/helm/helm/issues/3264
# * https://github.com/helm/helm/issues/3612
# cur_hash=($(md5sum ${tgz}))
# echo $cur_hash
# remote_hash=$(aws s3api head-object --bucket public.wire.com --key charts/${tgz} | jq '.ETag' -r| tr -d '"')
# echo $remote_hash
# if [ "$cur_hash" != "$remote_hash" ]; then
#     echo "ERROR: Current hash should be the same as the remote hash. Please bump the version of chart {$chart}."
#     exit 1
# fi
