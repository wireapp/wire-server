#!/usr/bin/env bash

set -eu

# lint all shell scripts with ShellCheck
# FUTUREWORK: Fix issues of the explicitly (no globbing) excluded files.

mapfile -t SHELL_FILES_TO_LINT < <(
    git ls-files |
        grep "\.sh$" |
        grep -v "dist-newstyle/" |
        grep -v "services/nginz/third_party/" |
        grep -v "libs/wire-api/test/golden/gentests.sh" |
        grep -v "changelog.d/mk-changelog.sh" |
        grep -v "hack/bin/integration-teardown.sh" |
        grep -v "hack/bin/diff-failure.sh" |
        grep -v "hack/bin/integration-setup.sh" |
        grep -v "hack/bin/cabal-run-tests.sh" |
        grep -v "hack/bin/integration-teardown-federation.sh" |
        grep -v "hack/bin/integration-setup-federation.sh" |
        grep -v "hack/bin/serve-charts.sh" |
        grep -v "hack/bin/cabal-install-artefacts.sh" |
        grep -v "hack/bin/helm-template.sh" |
        grep -v "hack/bin/set-chart-image-version.sh" |
        grep -v "hack/bin/copy-charts.sh" |
        grep -v "hack/bin/set-helm-chart-version.sh" |
        grep -v "hack/bin/integration-spring-cleaning.sh" |
        grep -v "hack/bin/upload-helm-charts-s3.sh" |
        grep -v "hack/bin/integration-test-logs.sh" |
        grep -v "services/nginz/nginz_reload.sh" |
        grep -v "services/spar/test-scim-suite/mk_collection.sh" |
        grep -v "services/spar/test-scim-suite/runsuite.sh" |
        grep -v "services/spar/test-scim-suite/run.sh" |
        grep -v "services/brig/federation-tests.sh" |
        grep -v "services/integration.sh" |
        grep -v "deploy/services-demo/create_test_team_members.sh" |
        grep -v "deploy/services-demo/demo.sh" |
        grep -v "deploy/services-demo/create_test_team_scim.sh" |
        grep -v "deploy/services-demo/create_test_user.sh" |
        grep -v "deploy/services-demo/create_team_members.sh" |
        grep -v "deploy/services-demo/register_idp_internal.sh" |
        grep -v "deploy/services-demo/create_test_team_admins.sh" |
        grep -v "deploy/dockerephemeral/init.sh" |
        grep -v "tools/nginz_disco/nginz_disco.sh" |
        grep -v "tools/rebase-onto-formatter.sh" |
        grep -v "tools/sftd_disco/sftd_disco.sh" |
        grep -v "tools/ormolu.sh" |
        grep -v "tools/db/move-team/dump_merge_teams.sh"
)

shellcheck -x "${SHELL_FILES_TO_LINT[@]}"
