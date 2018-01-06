 * Nginz is not in place
 * Some localstack services work only partially at the moment:
   * https://github.com/localstack/localstack/issues/314
   * When brig tries to read from the local SQS
```
localstack_sqs_1       | 2018-01-06T19:23:57:ERROR:localstack.services.generic_proxy: Error forwarding request: [Errno 32] Broken pipe Traceback (most recent call last):
localstack_sqs_1       |   File "/opt/code/localstack/localstack/services/generic_proxy.py", line 201, in forward
localstack_sqs_1       |     self.send_response(response.status_code)
localstack_sqs_1       |   File "/usr/lib/python2.7/BaseHTTPServer.py", line 406, in send_response
localstack_sqs_1       |     self.send_header('Server', self.version_string())
localstack_sqs_1       |   File "/usr/lib/python2.7/BaseHTTPServer.py", line 412, in send_header
localstack_sqs_1       |     self.wfile.write("%s: %s\r\n" % (keyword, value))
localstack_sqs_1       |   File "/usr/lib/python2.7/socket.py", line 328, in write
localstack_sqs_1       |     self.flush()
localstack_sqs_1       |   File "/usr/lib/python2.7/socket.py", line 307, in flush
localstack_sqs_1       |     self._sock.sendall(view[write_offset:write_offset+buffer_size])
localstack_sqs_1       |   File "/usr/lib/python2.7/ssl.py", line 753, in sendall
localstack_sqs_1       |     v = self.send(data[count:])
localstack_sqs_1       |   File "/usr/lib/python2.7/ssl.py", line 719, in send
localstack_sqs_1       |     v = self._sslobj.write(data)
localstack_sqs_1       | error: [Errno 32] Broken pipe
localstack_sqs_1       | 
localstack_sqs_1       | 
localstack_sqs_1       | Exception happened during processing of request from ('172.20.0.11', 41394)
```
