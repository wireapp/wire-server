# RabbitMQ to NATS Migration - Quick Start

## 🎯 What's in This PR

This PR provides the **foundation** for migrating Wire server from RabbitMQ to NATS. It includes:

1. **NATS Client Library** - Core implementation for NATS messaging
2. **Infrastructure Updates** - Docker Compose with NATS instead of RabbitMQ
3. **Documentation** - Comprehensive guides and checklists
4. **Testing Tools** - Scripts to validate NATS connectivity

## ⚠️ Important: Services Not Yet Migrated

**All Wire services still use RabbitMQ.** This PR only provides the foundation. Service migration is planned for future PRs.

## 📁 Key Files

| File | Description |
|------|-------------|
| `NATS_MIGRATION_STATUS.md` | Current migration status and what's completed |
| `docs/rabbitmq-to-nats-migration.md` | Detailed migration guide with architecture info |
| `docs/nats-migration-checklist.md` | Step-by-step checklist for migrating each service |
| `docs/nats-config-example.yaml` | Configuration examples for NATS |
| `libs/extended/src/Network/NATS/Client.hs` | NATS client implementation |
| `libs/extended/src/Network/NATS/Extended.hs` | AMQP compatibility layer |
| `hack/bin/test-nats-connection.sh` | NATS connectivity test script |

## 🚀 Quick Start

### 1. Start NATS Server

```bash
cd deploy/dockerephemeral
export NATS_USERNAME=guest
export NATS_PASSWORD=guest
docker-compose up nats
```

### 2. Test Connection

```bash
./hack/bin/test-nats-connection.sh
```

### 3. Access NATS Monitoring

Open http://localhost:8222 in your browser to see:
- Server status
- Connection stats
- Message throughput
- Health checks

## 📚 Documentation

### For Understanding the Migration

1. **Start here**: [`NATS_MIGRATION_STATUS.md`](NATS_MIGRATION_STATUS.md)
   - What's done, what's not
   - Current limitations
   - Timeline estimates

2. **Deep dive**: [`docs/rabbitmq-to-nats-migration.md`](docs/rabbitmq-to-nats-migration.md)
   - Why NATS?
   - Architecture changes
   - Subject naming conventions
   - Testing strategies

### For Implementing the Migration

3. **Step-by-step**: [`docs/nats-migration-checklist.md`](docs/nats-migration-checklist.md)
   - Detailed checklist for each service
   - Pre-migration tasks
   - Testing procedures
   - Sign-off requirements

4. **Configuration**: [`docs/nats-config-example.yaml`](docs/nats-config-example.yaml)
   - Service configuration examples
   - Kubernetes/Helm values
   - Environment variables

## 🏗️ Architecture

### Before (RabbitMQ)
```
[Service] --AMQP--> [RabbitMQ Exchange] --> [Queue] --> [Consumer]
```

### After (NATS)
```
[Service] --NATS--> [Subject: user.notifications.{userId}] --> [Subscriber]
```

### Key Differences

| Aspect | RabbitMQ | NATS |
|--------|----------|------|
| Routing | Exchange + Queue | Subject-based |
| Persistence | Built-in | JetStream (future) |
| Complexity | High | Low |
| Performance | Moderate | High |
| Operations | Complex | Simple |

## 🔄 Migration Phases

### ✅ Phase 1: Foundation (Current PR)
- NATS client library
- Infrastructure updates
- Documentation

### 🔜 Phase 2: Service Migration (4-6 weeks)
1. background-worker
2. gundeck
3. cannon
4. brig
5. galley

### 🔜 Phase 3: Enhanced Features (2-3 weeks)
- JetStream persistence
- TLS support
- NATS clustering
- Admin API

### 🔜 Phase 4: Infrastructure (2 weeks)
- Helm charts
- Nix configuration
- Integration tests
- CI/CD updates

### 🔜 Phase 5: Production (2-3 weeks)
- Performance testing
- Security review
- Monitoring setup
- Deployment

## 🧪 Testing

### Local Testing

```bash
# Start NATS
cd deploy/dockerephemeral
docker-compose up nats

# In another terminal, test connection
./hack/bin/test-nats-connection.sh

# Access monitoring UI
open http://localhost:8222
```

### NATS CLI (optional)

```bash
# Install NATS CLI (if available)
# Mac: brew install nats-io/nats-tools/nats
# Linux: Download from https://github.com/nats-io/natscli

# Subscribe to a test subject
nats sub "test.subject"

# Publish to a test subject
nats pub "test.subject" "Hello NATS"
```

## 📊 Current Status

```
Foundation:    ████████████████████ 100% ✅
Services:      ░░░░░░░░░░░░░░░░░░░░   0% 🔜
Features:      ░░░░░░░░░░░░░░░░░░░░   0% 🔜
Infrastructure:░░░░░░░░░░░░░░░░░░░░   0% 🔜
Production:    ░░░░░░░░░░░░░░░░░░░░   0% 🔜
                                        
Overall:       ████░░░░░░░░░░░░░░░░  20%
```

## 🎯 Next Steps

1. **Review this PR**
   - Check NATS client implementation
   - Review documentation
   - Test NATS connectivity

2. **Plan Phase 2**
   - Assign developers to service migration
   - Set up development environments
   - Schedule sprint planning

3. **Start Migration**
   - Follow `docs/nats-migration-checklist.md`
   - Begin with background-worker (simplest)
   - Iterate and improve

## ⚙️ Configuration Changes

### Environment Variables (New)

```bash
export NATS_USERNAME=guest
export NATS_PASSWORD=guest
export NATS_HOST=localhost
export NATS_PORT=4222
```

### Service Configuration (Future)

When services are migrated, configuration will change from:

```yaml
# Old
rabbitmq:
  host: localhost
  port: 5672
  vHost: /
```

To:

```yaml
# New
nats:
  host: localhost
  port: 4222
  namespace: "wire"
```

## 🐛 Known Limitations

1. **No JetStream**: Current implementation uses basic NATS (ephemeral)
2. **No TLS**: TLS support planned for Phase 3
3. **No Clustering**: Single NATS server only
4. **Basic Auth**: Username/password only
5. **Services Not Migrated**: All services still use RabbitMQ

## 📞 Support

### Questions?

1. Check the documentation:
   - `NATS_MIGRATION_STATUS.md` - Current status
   - `docs/rabbitmq-to-nats-migration.md` - Detailed guide
   - `docs/nats-migration-checklist.md` - Step-by-step checklist

2. NATS Resources:
   - [NATS Documentation](https://docs.nats.io/)
   - [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol)
   - [JetStream](https://docs.nats.io/nats-concepts/jetstream)

3. Contact:
   - Open an issue in the repository
   - Contact the backend team

## 🔒 Security Notes

- Default credentials (`guest/guest`) are for development only
- Production should use:
  - Strong passwords from secrets management
  - TLS encryption (Phase 3)
  - Network policies
  - Authentication tokens

## 📈 Performance

NATS generally offers:
- **Lower latency** than RabbitMQ
- **Higher throughput**
- **Lower memory footprint**
- **Better CPU efficiency**

Specific benchmarks will be conducted in Phase 5.

## 🚨 Rollback

If issues arise:
1. Keep RabbitMQ infrastructure during transition
2. Services can be rolled back individually
3. Configuration can revert to RabbitMQ
4. Messages in flight may be lost (plan accordingly)

## ✅ Sign-Off

This PR should be reviewed by:
- [ ] Backend team lead
- [ ] Service owners
- [ ] Operations team
- [ ] Architecture team

## 🎉 Conclusion

This PR provides a solid foundation for the NATS migration. While services aren't yet migrated, all the infrastructure and documentation is in place to begin the migration process systematically and safely.

The migration is estimated to take **10-14 weeks** with a dedicated team, and this foundation represents the first **1-2 weeks** of work.

Let's build something great! 🚀
