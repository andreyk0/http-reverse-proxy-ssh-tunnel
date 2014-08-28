http-reverse-proxy-ssh-tunnel
====

```
$ http-reverse-proxy-ssh-tunnel --help

A quick SSH tunnel hack for proxying HTTP service calls to another environment.

http-reverse-proxy-ssh-tunnel [OPTIONS]
  Usage: http-reverse-proxy-ssh-tunnel [--listen 8080] --gateway
  my.gateway.host

Common flags:
  -l --listen=INT    local port to bind to, defaults to 8080
  -g --gateway=ITEM  SSH gateway host
  -? --help          Display help message
  -V --version       Print version information

Source: https://github.com/andreyk0/http-reverse-proxy-ssh-tunnel.git

Proxies calls like 'http://svc-something:LISTEN_PORT/' to another environment
via SSH gateway.

This allows use of canonical service URLs in development environment.
E.g. settings listed below should make 'http://svc-something/' work on local
host.

Parses /etc/hosts, looking for entries below this comment

# http-reverse-proxy-ssh-tunnel

127.0.0.1 svc-something
127.0.0.1 svc-something-else

On a Mac you can run it on a non-privileged port and forward port 80
connections to it with
$ sudo ipfw add 100 fwd '127.0.0.1,8080' tcp from 127.0.0.1 to any 80 in
$ sudo ipfw show
```
