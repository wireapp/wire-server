#!/usr/bin/env bash

# Test NATS connection
# This script tests the NATS server connectivity and basic functionality

set -e

NATS_HOST="${NATS_HOST:-localhost}"
NATS_PORT="${NATS_PORT:-4222}"
NATS_MONITOR_PORT="${NATS_MONITOR_PORT:-8222}"

echo "Testing NATS connection..."
echo "Host: $NATS_HOST"
echo "Port: $NATS_PORT"
echo "Monitor Port: $NATS_MONITOR_PORT"
echo ""

# Check if NATS is running
echo "1. Checking if NATS server is accessible..."
if command -v nc &> /dev/null; then
    if nc -z "$NATS_HOST" "$NATS_PORT" 2>/dev/null; then
        echo "✓ NATS server is accessible on $NATS_HOST:$NATS_PORT"
    else
        echo "✗ Cannot connect to NATS server on $NATS_HOST:$NATS_PORT"
        echo "  Make sure NATS is running: docker-compose up nats"
        exit 1
    fi
else
    echo "⚠ netcat (nc) not found, skipping port check"
fi

# Check monitoring endpoint
echo ""
echo "2. Checking NATS monitoring endpoint..."
if command -v curl &> /dev/null; then
    if curl -s "http://$NATS_HOST:$NATS_MONITOR_PORT/healthz" > /dev/null 2>&1; then
        echo "✓ NATS monitoring is accessible at http://$NATS_HOST:$NATS_MONITOR_PORT"
        
        # Get NATS info
        echo ""
        echo "3. NATS Server Information:"
        curl -s "http://$NATS_HOST:$NATS_MONITOR_PORT/varz" | \
            grep -E '"(version|max_connections|max_payload|connections|in_msgs|out_msgs)"' | \
            head -8 || echo "Could not retrieve server info"
    else
        echo "⚠ NATS monitoring endpoint not accessible"
        echo "  This is normal if monitoring is disabled"
    fi
else
    echo "⚠ curl not found, skipping monitoring check"
fi

echo ""
echo "4. Testing basic NATS protocol..."

# Try to connect using telnet or nc
if command -v telnet &> /dev/null; then
    echo "Attempting connection test..."
    # Note: This is a very basic test. Real testing should use NATS client
    (
        sleep 1
        echo "PING"
        sleep 1
    ) | timeout 3 telnet "$NATS_HOST" "$NATS_PORT" 2>/dev/null | grep -q "PONG" && \
        echo "✓ Basic NATS protocol test passed" || \
        echo "⚠ Could not complete protocol test (this may be normal)"
else
    echo "⚠ telnet not found, skipping protocol test"
fi

echo ""
echo "All basic checks complete!"
echo ""
echo "Next steps:"
echo "  1. Check NATS monitoring dashboard: http://$NATS_HOST:$NATS_MONITOR_PORT"
echo "  2. Test with a NATS client: nats sub test.subject"
echo "  3. Publish a test message: nats pub test.subject 'Hello NATS'"
echo ""
