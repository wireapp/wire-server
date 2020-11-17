TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
DIR="${TOP_LEVEL}/services"

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

# multi-line sed: replace <something> in the following block with 'cassandra-ephemeral'
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

make -C "$DIR/gundeck" db-migrate
make -C "$DIR/galley" db-migrate
make -C "$DIR/spar" db-migrate
make -C "$DIR/brig" db-migrate
