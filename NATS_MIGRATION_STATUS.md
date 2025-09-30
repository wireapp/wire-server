# NATS Migration Status

## Overview

This document tracks the progress of migrating Wire server from RabbitMQ (AMQP) to NATS messaging system.

## Current Status: Phase 1 - Foundation ✅

### What's Completed

1. **NATS Client Library** (`libs/extended/src/Network/NATS/Client.hs`)
   - Basic NATS protocol implementation
   - Connection management
   - Publish/Subscribe functionality
   - Message handling infrastructure

2. **AMQP Compatibility Layer** (`libs/extended/src/Network/NATS/Extended.hs`)
   - Interface compatible with existing `Network.AMQP.Extended`
   - Connection retry logic
   - Lifecycle hooks for connection management
   - Credentials from environment variables

3. **Docker Infrastructure**
   - `docker-compose.yaml` updated to use NATS instead of RabbitMQ
   - NATS 2.10 with JetStream enabled
   - Monitoring interface on port 8222

4. **Documentation**
   - Migration guide: `docs/rabbitmq-to-nats-migration.md`
   - Configuration options updated
   - Environment variable documentation

## What's NOT Done Yet

### ⚠️ Important: Services Have NOT Been Migrated

The following services still use RabbitMQ and need migration in future PRs:

- [ ] **gundeck** - Push notification service
- [ ] **cannon** - WebSocket connection service  
- [ ] **background-worker** - Federation notification processor
- [ ] **brig** - User management service
- [ ] **galley** - Conversation service

### Missing Features

The current NATS implementation lacks:

- [ ] JetStream support (for persistent queues)
- [ ] TLS/SSL connections
- [ ] NATS clustering support
- [ ] Admin/management API client
- [ ] Comprehensive error handling
- [ ] Performance optimizations
- [ ] Integration tests

### Infrastructure Updates Needed

- [ ] Helm charts (all services)
- [ ] Nix build configuration
- [ ] CI/CD pipeline updates
- [ ] Integration test infrastructure
- [ ] Monitoring and metrics setup

## Building and Testing

### Prerequisites

The NATS client requires these Haskell packages:
- `network` - Network sockets
- `random` - Random number generation
- `aeson` - JSON parsing
- `bytestring` - Byte string operations
- `containers` - Map/Set data structures

### Local Testing

Start NATS server:
```bash
cd deploy/dockerephemeral
export NATS_USERNAME=guest
export NATS_PASSWORD=guest
docker-compose up nats
```

NATS endpoints:
- Client: `localhost:4222`
- Monitoring: `http://localhost:8222`
- Health: `http://localhost:8222/healthz`

### Compilation

The NATS modules are added to `libs/extended/extended.cabal`:
```
exposed-modules:
    Network.NATS.Client
    Network.NATS.Extended
```

Build the extended library:
```bash
cabal build extended
```

## Next Steps

### Phase 2: Service Migration (Estimated: 4-6 weeks)

Migrate services one at a time:

1. **Start with Background Worker** (simplest)
   - Single consumer pattern
   - Well-defined queue structure
   - Good test case for approach

2. **Then Gundeck** (moderate complexity)
   - Publishing side mostly
   - Well-isolated

3. **Then Cannon** (most complex)
   - Per-client queue creation
   - Complex connection pooling
   - Critical path for notifications

4. **Finally Brig and Galley**
   - Event publishing
   - Less complex than Cannon

### Phase 3: Enhanced Features (Estimated: 2-3 weeks)

- Add JetStream for persistence
- Implement TLS support
- Add clustering capabilities
- Build admin API client
- Improve error handling

### Phase 4: Infrastructure (Estimated: 2 weeks)

- Update all Helm charts
- Update Nix configurations
- Migrate test infrastructure
- Update CI/CD pipelines

### Phase 5: Production Readiness (Estimated: 2-3 weeks)

- Performance testing
- Load testing
- Security review
- Monitoring setup
- Migration strategy
- Rollback procedures

## Risks and Mitigation

### Risk: Service Compatibility

**Mitigation:** 
- Compatibility layer maintains similar interface
- Gradual service-by-service migration
- Ability to run both systems temporarily

### Risk: Message Delivery Semantics

**Problem:** NATS and RabbitMQ have different message delivery guarantees

**Mitigation:**
- Use JetStream for persistence needs
- Document semantic differences
- Update application logic as needed

### Risk: Performance Under Load

**Mitigation:**
- Benchmark early and often
- Use NATS best practices
- Monitor performance metrics

### Risk: Operational Complexity

**Mitigation:**
- Comprehensive documentation
- Training for ops team
- Gradual rollout strategy

## Decision Log

### Why NATS?

1. **Simpler Operations**: Less infrastructure complexity than RabbitMQ
2. **Better Performance**: Lower latency, higher throughput
3. **Cloud Native**: Better fit for Kubernetes deployments
4. **Lower Resource Usage**: Smaller memory and CPU footprint
5. **Built-in Clustering**: Simpler to scale horizontally

### Why Not Keep RabbitMQ?

1. Operational overhead
2. Complex message routing not fully utilized
3. Resource intensive
4. Scaling challenges in Kubernetes

## Timeline

- **Phase 1 (Foundation)**: ✅ Complete
- **Phase 2 (Services)**: Not started - Estimated 4-6 weeks
- **Phase 3 (Features)**: Not started - Estimated 2-3 weeks
- **Phase 4 (Infrastructure)**: Not started - Estimated 2 weeks
- **Phase 5 (Production)**: Not started - Estimated 2-3 weeks

**Total Estimated Time: 10-14 weeks** with a dedicated team

## Getting Help

- Review `docs/rabbitmq-to-nats-migration.md` for detailed migration guide
- Check NATS documentation: https://docs.nats.io/
- Review NATS protocol: https://docs.nats.io/reference/reference-protocols/nats-protocol

## Contributing

When working on the migration:

1. Follow existing code patterns
2. Add comprehensive tests
3. Update documentation
4. Consider backward compatibility
5. Review changes with the team

## Questions?

Contact the backend team or open an issue in the repository.
