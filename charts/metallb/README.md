# IMPORTANT
#
# Make sure you have a SINGLE metallb instance across the whole cluster, do not create multiple of these
# Have a read at the warning here: https://metallb.universe.tf/installation/
#
# You only need to adjust values to the available IP address range and run
#
# helm upgrade --install --namespace metallb-system metallb charts/metallb \
#    -f values/metallb/values.yaml --wait --timeout 1800
#
