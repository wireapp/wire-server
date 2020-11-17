USAGE="$0 <your name>"
export NAMESPACE=${1:?$USAGE}-dev

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
DIR="${TOP_LEVEL}/services"

function change_config_files() {
    galley="$DIR/galley/galley.integration.yaml"
    brig="$DIR/brig/brig.integration.yaml"
    gundeck="$DIR/gundeck/gundeck.integration.yaml"
    cargohold="$DIR/cargohold/cargohold.integration.yaml"
    spar="$DIR/spar/spar.integration.yaml"
    # federator="$DIR/federator/federator.integration.yaml"
    # proxy="$DIR/proxy/proxy.integration.yaml"
    # cannon="$DIR/cannon/cannon.integration.yaml"

    sqs="s=localhost:4568=fake-aws-sqs:4568=g"
    sns="s=localhost:4575=fake-aws-sns:4575=g"
    ses="s=localhost:4569=fake-aws-ses:4569=g"
    dynamodb="s=localhost:4567=fake-aws-dynamodb:4567=g"
    s3="s=localhost:4570=fake-aws-s3:9000=g"

    es="s=127.0.0.1:9200=elasticsearch-ephemeral:9200=g"

    # multi-line sed: replace <something> in the following block with 'cassandra-ephemeral'
    # endpoint:
    #   host: <something>
    cassandra="/^ *endpoint:/,/^ *[^:]*:/s/host: .*/host: cassandra-ephemeral/"

    # multi-line sed: replace <something> in the following block with 'redis-ephemeral'
    # redis:
    #   host: <something>
    redis="/^ *redis:/,/^ *[^:]*:/s/host: .*/host: redis-ephemeral/"

    sed -i "$sqs" "$brig"
    sed -i "$sqs" "$galley"
    sed -i "$sqs" "$gundeck"

    sed -i "$sns" "$gundeck"
    sed -i "$redis" "$gundeck"

    sed -i "$s3" "$cargohold"

    sed -i "$ses" "$brig"
    sed -i "$dynamodb" "$brig"
    sed -i "$es" "$brig"

    sed -i "$cassandra" "$brig"
    sed -i "$cassandra" "$galley"
    sed -i "$cassandra" "$gundeck"
    sed -i "$cassandra" "$spar"
}

function setup_dev_environment() {
    echo TODO setup dev environment
    # integration-setup.sh for fake-aws and databases-ephemeral only
}

setup_dev_environment
change_config_files

telepresence --namespace "$NAMESPACE" --also-proxy cassandra-ephemeral --run bash -c "cd $TOP_LEVEL; make db-migrate; echo ''; echo 'telepresence proxy up (curl http://elasticsearch-ephemeeral:9200 should work) - you can now run integration tests.'; echo 'Press Control+C to stop the telepresence'; sleep 100000000"
