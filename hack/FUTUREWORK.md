
* Set imagePullPolicy to Always in hack/helm_vars so kubernetes pulls local updates.
* modify integration-cleanup.sh to not do awk/grep parsing as it now supports other output formats.
* make hack/integration-cleanup less agressive, and don't let it delete developer environments. This requires a kubernetes cluster with enough resources to potentialy deal with a range of stale environments.
