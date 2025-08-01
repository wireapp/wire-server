# PostgreSQL External Service Helm Chart

This Helm chart creates headless Kubernetes `Service`s with custom `Endpoint`s to connect to external PostgreSQL instances in an HA configuration (one primary and multiple replicas).

## Features

- Creates two separate services for read-write (primary) and read-only (replica) PostgreSQL connections
- Uses headless services for direct DNS resolution to PostgreSQL instances
- Supports different IP lists for read-write and read-only endpoints

## Configuration

Configure the chart in your `values.yaml`:

```yaml
portPostgresql: 5432  # PostgreSQL port number

# List of IP addresses for read-write (primary) PostgreSQL instance
RWIPs:
  - <primary-ip>

# List of IP addresses for read-only (replica) PostgreSQL instances
ROIPs:
  - <replica-ip-1>
  - <replica-ip-2>
