# RabbitMQ to NATS Migration Guide

## Overview

This document describes the migration from RabbitMQ (AMQP) to NATS for the Wire server messaging infrastructure.

## Why NATS?

NATS is a lightweight, high-performance messaging system that offers:
- Simpler operational model
- Better performance for request-reply patterns
- Native cloud-native deployment support
- Built-in JetStream for persistence when needed
- Lower resource footprint

## Architecture Changes

### Message Semantics

| Feature | RabbitMQ | NATS |
|---------|----------|------|
| Delivery | Persistent queues | Ephemeral (JetStream for persistence) |
| Acknowledgment | Built-in ACK/NACK | JetStream ACK |
| Routing | Exchange + Queue bindings | Subject-based |
| Dead Letter Queue | Built-in | JetStream consumers |

### Subject Naming Convention

NATS uses subject-based routing instead of exchanges and queues. The migration uses the following convention:

- User notifications: `user.notifications.{userId}.{clientId}`
- Backend notifications: `backend.notifications.{domain}`
- Cells events: `cells.events`
- Temporary clients: `user.notifications.{userId}.temp`

### Connection Management

The NATS client implementation provides:
- Automatic reconnection with exponential backoff
- Connection pooling (similar to RabbitMQ channels)
- Lifecycle hooks for connection management

## Implementation Status

### Completed
- [x] Basic NATS client implementation (`libs/extended/src/Network/NATS/Client.hs`)
- [x] AMQP compatibility layer (`libs/extended/src/Network/NATS/Extended.hs`)
- [x] Docker Compose configuration updated

### In Progress
- [ ] Service migrations (gundeck, cannon, background-worker, brig, galley)
- [ ] Helm chart updates
- [ ] Nix build configuration
- [ ] Integration tests
- [ ] Performance testing

### Pending
- [ ] JetStream integration for persistent queues
- [ ] NATS admin client (replacing RabbitMqAdmin)
- [ ] Migration tooling
- [ ] Monitoring and metrics
- [ ] Production deployment strategy

## Current Limitations

The initial NATS client implementation has several limitations:

1. **Basic Protocol**: Implements core NATS protocol only, not JetStream
2. **No Persistence**: Messages are ephemeral by default
3. **Simple Authentication**: Username/password only
4. **No TLS**: TLS support needs to be added
5. **No Clustering**: Single server connection only

These limitations will be addressed in subsequent phases.

## Migration Strategy

### Phase 1: Foundation (Current)
Create basic NATS client and compatibility layer.

### Phase 2: Service Migration
Migrate each service individually:
1. gundeck (push notifications)
2. cannon (websocket)
3. background-worker (federation)
4. brig (user events)
5. galley (conversation events)

### Phase 3: Enhanced Features
- Add JetStream support
- Implement TLS
- Add clustering support
- Improve error handling

### Phase 4: Production Deployment
- Performance testing
- Load testing
- Gradual rollout
- Monitoring

## Configuration

### Environment Variables

```bash
# NATS connection (replaces RABBITMQ_* vars)
export NATS_USERNAME=guest
export NATS_PASSWORD=guest
```

### Service Configuration

Services need to be updated with NATS configuration instead of RabbitMQ:

```yaml
# Old (RabbitMQ)
rabbitmq:
  host: localhost
  port: 5672
  vHost: /

# New (NATS)
nats:
  host: localhost
  port: 4222
  namespace: ""  # Optional subject prefix
```

## Testing

### Local Development

```bash
# Start NATS using docker-compose
cd deploy/dockerephemeral
docker-compose up nats

# NATS will be available at:
# - Client port: 4222
# - Monitoring: http://localhost:8222
```

### Integration Tests

Integration tests need to be updated to:
1. Use NATS instead of RabbitMQ
2. Update queue assertions
3. Handle NATS-specific behavior

## Rollback Plan

During the migration, the system can be rolled back by:
1. Reverting service deployments
2. Switching back to RabbitMQ in configuration
3. Ensuring data loss is acceptable (messages in flight will be lost)

## Performance Considerations

NATS generally offers:
- Lower latency than RabbitMQ
- Higher throughput
- Lower memory footprint
- Better CPU efficiency

However, specific benchmarks should be conducted for the Wire use case.

## Monitoring

Key metrics to monitor:
- Connection status
- Message throughput
- Message latency
- Error rates
- Connection pool utilization

## Support and Documentation

- [NATS Documentation](https://docs.nats.io/)
- [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol)
- [JetStream](https://docs.nats.io/nats-concepts/jetstream)

## Contributing

When working on the migration:
1. Follow the existing code style
2. Add tests for new functionality
3. Update documentation
4. Consider backward compatibility
5. Test thoroughly before submitting PRs

## Questions and Issues

For questions or issues related to the migration, please:
1. Check this document first
2. Review NATS documentation
3. Open an issue in the repository
4. Contact the backend team
