# NATS Migration Checklist

This document provides a step-by-step checklist for migrating Wire server services from RabbitMQ to NATS.

## Pre-Migration

### Planning
- [ ] Review migration guide (`docs/rabbitmq-to-nats-migration.md`)
- [ ] Understand NATS fundamentals (https://docs.nats.io/)
- [ ] Review current RabbitMQ usage patterns
- [ ] Identify all services using RabbitMQ
- [ ] Document current message flows
- [ ] Plan migration order (background-worker → gundeck → cannon → brig/galley)
- [ ] Allocate development resources (2-3 months)
- [ ] Set up staging environment

### Environment Setup
- [ ] Install NATS server (locally or staging)
- [ ] Install NATS CLI tools (`nats` command)
- [ ] Configure NATS credentials
- [ ] Set up NATS monitoring
- [ ] Test NATS connectivity using `hack/bin/test-nats-connection.sh`

## Phase 1: Foundation (Completed ✅)

- [x] Create NATS client library (`libs/extended/src/Network/NATS/Client.hs`)
- [x] Create compatibility layer (`libs/extended/src/Network/NATS/Extended.hs`)
- [x] Update `extended.cabal` with NATS modules
- [x] Update docker-compose to use NATS
- [x] Update documentation
- [x] Create migration guides

## Phase 2: Service Migration

### Background Worker (Start Here - Simplest)

- [ ] **Analyze Current Code**
  - [ ] Review `services/background-worker/src/Wire/BackendNotificationPusher.hs`
  - [ ] Review `services/background-worker/src/Wire/BackgroundWorker/Env.hs`
  - [ ] Document current RabbitMQ usage
  - [ ] Identify queue names and routing keys

- [ ] **Update Dependencies**
  - [ ] Update `services/background-worker/background-worker.cabal`
  - [ ] Replace `amqp` dependency (or keep for transition)
  - [ ] Add `extended` library with NATS support

- [ ] **Update Options/Configuration**
  - [ ] Modify `Wire.BackgroundWorker.Options`
  - [ ] Add NATS configuration parsing
  - [ ] Support both RabbitMQ and NATS (transition period)

- [ ] **Update Environment**
  - [ ] Modify `Wire.BackgroundWorker.Env`
  - [ ] Replace RabbitMQ client with NATS client
  - [ ] Update connection initialization

- [ ] **Update Core Logic**
  - [ ] Replace AMQP subscribe with NATS subscribe
  - [ ] Update message handling
  - [ ] Update acknowledgment logic
  - [ ] Handle NATS-specific errors

- [ ] **Testing**
  - [ ] Update unit tests
  - [ ] Update integration tests in `services/background-worker/test/`
  - [ ] Test with local NATS server
  - [ ] Test reconnection logic
  - [ ] Test message delivery

- [ ] **Configuration Files**
  - [ ] Update `background-worker.integration.yaml`
  - [ ] Update other YAML configs in `deploy/dockerephemeral/`

### Gundeck (Second - Moderate Complexity)

- [ ] **Analyze Current Code**
  - [ ] Review `services/gundeck/src/Gundeck/Push.hs`
  - [ ] Review `services/gundeck/src/Gundeck/Options.hs`
  - [ ] Review `services/gundeck/src/Gundeck/Env.hs`
  - [ ] Document message publishing patterns

- [ ] **Update Dependencies**
  - [ ] Update `services/gundeck/gundeck.cabal`
  - [ ] Handle `amqp` dependency transition

- [ ] **Update Options/Configuration**
  - [ ] Modify `Gundeck.Options`
  - [ ] Add NATS endpoint configuration
  - [ ] Update settings for cells event queue

- [ ] **Update Environment**
  - [ ] Modify `Gundeck.Env`
  - [ ] Replace RabbitMQ channel with NATS connection
  - [ ] Update connection management

- [ ] **Update Publishing Logic**
  - [ ] Update `mpaPublishToRabbitMq` in `Gundeck.Push`
  - [ ] Convert RabbitMQ queue names to NATS subjects
  - [ ] Update message serialization if needed

- [ ] **Testing**
  - [ ] Update unit tests in `services/gundeck/test/unit/`
  - [ ] Update mock in `MockGundeck.hs`
  - [ ] Integration testing
  - [ ] Performance testing

- [ ] **Configuration Files**
  - [ ] Update `gundeck.integration.yaml`
  - [ ] Update deployment configs

### Cannon (Third - Most Complex)

- [ ] **Analyze Current Code**
  - [ ] Review `services/cannon/src/Cannon/RabbitMq.hs`
  - [ ] Review `services/cannon/src/Cannon/RabbitMqConsumerApp.hs`
  - [ ] Review `services/cannon/src/Cannon/Run.hs`
  - [ ] Document channel pool architecture
  - [ ] Document queue creation patterns

- [ ] **Create NATS Equivalent**
  - [ ] Create `services/cannon/src/Cannon/Nats.hs` (replace RabbitMq.hs)
  - [ ] Implement connection pooling for NATS
  - [ ] Handle per-client queue creation (NATS subjects)

- [ ] **Update Consumer App**
  - [ ] Create `services/cannon/src/Cannon/NatsConsumerApp.hs`
  - [ ] Port WebSocket integration
  - [ ] Update message delivery logic
  - [ ] Handle acknowledgments with NATS

- [ ] **Update Dependencies**
  - [ ] Update `services/cannon/cannon.cabal`
  - [ ] Manage `amqp` dependency

- [ ] **Update Options**
  - [ ] Modify `Cannon.Options`
  - [ ] Add NATS pool configuration
  - [ ] Update drain options if needed

- [ ] **Update Run Logic**
  - [ ] Modify `Cannon.Run`
  - [ ] Initialize NATS pool instead of RabbitMQ pool
  - [ ] Update drain behavior

- [ ] **Testing**
  - [ ] Extensive testing required (most complex service)
  - [ ] Test connection pool behavior
  - [ ] Test client connection/disconnection
  - [ ] Test message delivery under load
  - [ ] Test drain functionality

- [ ] **Configuration Files**
  - [ ] Update `cannon.integration.yaml` and `cannon2.integration.yaml`
  - [ ] Update deployment configs

### Brig (Fourth - Publishing Only)

- [ ] **Analyze Current Code**
  - [ ] Review `services/brig/src/Brig/App.hs`
  - [ ] Review `services/brig/src/Brig/Options.hs`
  - [ ] Identify event publishing locations

- [ ] **Update Configuration**
  - [ ] Modify `Brig.Options`
  - [ ] Add NATS configuration

- [ ] **Update Event Publishing**
  - [ ] Replace RabbitMQ publishing with NATS
  - [ ] Update event serialization if needed

- [ ] **Testing**
  - [ ] Update integration tests
  - [ ] Verify event delivery

- [ ] **Configuration Files**
  - [ ] Update `brig.integration.yaml`
  - [ ] Update deployment configs

### Galley (Fifth - Publishing Only)

- [ ] **Analyze Current Code**
  - [ ] Review `services/galley/src/Galley/App.hs`
  - [ ] Review `services/galley/src/Galley/Env.hs`
  - [ ] Review `services/galley/src/Galley/Intra/BackendNotificationQueue.hs`

- [ ] **Update Configuration**
  - [ ] Modify `Galley.Options`
  - [ ] Add NATS configuration

- [ ] **Update Backend Notification Queue**
  - [ ] Replace RabbitMQ queue with NATS subject
  - [ ] Update publishing logic

- [ ] **Testing**
  - [ ] Update integration tests
  - [ ] Verify backend notifications

- [ ] **Configuration Files**
  - [ ] Update `galley.integration.yaml`
  - [ ] Update deployment configs

## Phase 3: Enhanced Features

### JetStream Support
- [ ] Add JetStream consumer implementation
- [ ] Add JetStream publisher implementation
- [ ] Update configuration for JetStream
- [ ] Test persistence guarantees
- [ ] Update documentation

### TLS Support
- [ ] Add TLS configuration options
- [ ] Implement TLS in NATS client
- [ ] Test with TLS-enabled NATS
- [ ] Document TLS setup

### NATS Admin Client
- [ ] Create `Network.NatsAdmin` module
- [ ] Implement monitoring API calls
- [ ] Replace RabbitMqAdmin usage
- [ ] Update background-worker admin calls

### Error Handling
- [ ] Review and improve error handling
- [ ] Add retry policies
- [ ] Add circuit breakers if needed
- [ ] Improve logging

## Phase 4: Infrastructure

### Helm Charts
- [ ] Update `charts/background-worker/`
- [ ] Update `charts/gundeck/`
- [ ] Update `charts/cannon/`
- [ ] Update `charts/brig/`
- [ ] Update `charts/galley/`
- [ ] Create/update `charts/nats/` or use official NATS Helm chart
- [ ] Remove `charts/rabbitmq/` and `charts/rabbitmq-external/`

### Nix Configuration
- [ ] Update `nix/overlay.nix`
- [ ] Update service default.nix files
- [ ] Remove RabbitMQ dependencies where possible
- [ ] Add NATS dependencies
- [ ] Regenerate local nix packages: `hack/bin/generate-local-nix-packages.sh`

### Tools
- [ ] Migrate `tools/rabbitmq-consumer/` to `tools/nats-consumer/`
- [ ] Update or remove `nix/pkgs/rabbitmqadmin/`
- [ ] Create NATS testing/debugging tools

### Integration Tests
- [ ] Update `integration/test/Test/Events.hs`
- [ ] Update `integration/test/Test/Conversation.hs`
- [ ] Update `integration/test/Testlib/Env.hs`
- [ ] Update `integration/test/Testlib/ResourcePool.hs`
- [ ] Remove RabbitMQ-specific test code
- [ ] Add NATS-specific test utilities

### CI/CD
- [ ] Update GitHub Actions workflows
- [ ] Update test scripts in `Makefile`
- [ ] Update deployment scripts
- [ ] Add NATS to test environments

## Phase 5: Production Readiness

### Performance Testing
- [ ] Benchmark message throughput
- [ ] Benchmark message latency
- [ ] Compare with RabbitMQ baseline
- [ ] Identify bottlenecks
- [ ] Optimize as needed

### Load Testing
- [ ] Test under production-like load
- [ ] Test connection pool behavior
- [ ] Test failure scenarios
- [ ] Test recovery from outages

### Security Review
- [ ] Review authentication mechanism
- [ ] Review authorization (if applicable)
- [ ] Review TLS configuration
- [ ] Review secrets management
- [ ] Penetration testing

### Monitoring & Observability
- [ ] Set up NATS metrics collection
- [ ] Create Grafana dashboards
- [ ] Set up alerts for NATS
- [ ] Update runbooks
- [ ] Train ops team

### Documentation
- [ ] Update deployment documentation
- [ ] Update operational runbooks
- [ ] Create troubleshooting guide
- [ ] Update architecture diagrams
- [ ] Create training materials

### Migration Planning
- [ ] Create detailed migration plan
- [ ] Plan for dual-write period (if needed)
- [ ] Create rollback procedures
- [ ] Schedule maintenance windows
- [ ] Communicate with stakeholders

### Deployment
- [ ] Deploy to staging
- [ ] Validate in staging
- [ ] Deploy to canary environment
- [ ] Monitor canary closely
- [ ] Gradual rollout to production
- [ ] Monitor production metrics
- [ ] Complete rollout

### Post-Migration
- [ ] Remove RabbitMQ infrastructure
- [ ] Archive RabbitMQ documentation
- [ ] Clean up old code
- [ ] Remove deprecated configuration
- [ ] Update team knowledge base

## Rollback Procedures

If issues are encountered:

- [ ] Document rollback triggers
- [ ] Create rollback scripts
- [ ] Test rollback in staging
- [ ] Keep RabbitMQ infrastructure during transition
- [ ] Have RabbitMQ configs ready to restore

## Sign-off

Each phase should be signed off by:

- [ ] Development lead
- [ ] QA lead
- [ ] Operations lead
- [ ] Security team (for Phase 5)
- [ ] Product owner

## Notes

Use this space for notes, decisions, and issues encountered during migration:

```
Date       | Note
-----------|---------------------------------------------
2024-XX-XX | Started Phase 1 foundation work
2024-XX-XX | Completed NATS client implementation
...
```
