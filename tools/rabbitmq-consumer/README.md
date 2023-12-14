# RabbitMQ Consumer

```txt
rabbitmq-consumer

Usage: rabbitmq-consumer [-s|--host HOST] [-p|--port PORT]
                         [-u|--username USERNAME] [-w|--password PASSWORD]
                         [-v|--vhost VHOST] [-q|--queue QUEUE]
                         [-t|--timeout TIMEOUT] COMMAND

  CLI tool to consume messages from a RabbitMQ queue

Available options:
  -h,--help                Show this help text
  -s,--host HOST           RabbitMQ host (default: "localhost")
  -p,--port PORT           RabbitMQ Port (default: 5672)
  -u,--username USERNAME   RabbitMQ Username (default: "guest")
  -w,--password PASSWORD   RabbitMQ Password (default: "alpaca-grapefruit")
  -v,--vhost VHOST         RabbitMQ VHost (default: "/")
  -q,--queue QUEUE         RabbitMQ Queue (default: "test")
  -t,--timeout TIMEOUT     Timeout in seconds. The command will timeout if no
                           messages are received within this time. This can
                           happen when the queue is empty, or when we lose the
                           single active consumer race. (default: 10)

Available commands:
  head                     Print the first message in the queue
  drop-head                Drop the first message in the queue
  interactive              Interactively drop the first message from the queue
```
