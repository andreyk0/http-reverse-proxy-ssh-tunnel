http-reverse-proxy-ssh-tunnel
====

A quick SSH tunnel hack for proxying HTTP service calls to another environment.
Based on http-reverse-proxy examples.

The goal is to be able to execute curl commands with 'canonical' URLs locally
in a development environment.

E.g to make
```
curl -v http://svc-something/
curl -v http://svc-other/
```
work locally we point them to localhost with an /etc/hosts hack

```
$ cat /etc/hosts
........................

# http-reverse-proxy-ssh-tunnel

127.0.0.1 svc-something
127.0.0.1 svc-other
```

Start this process (on a non-privileged port, to avoid messing
with sudo and SSH agent and settings)
```
http-reverse-proxy-ssh-tunnel --listen 8080 --gateway my.gateway.host
```

And add (Mac-specific) ipfw rules to send all port 80 traffic to 8080:
```
$ sudo ipfw add 100 fwd 127.0.0.1,8080 tcp from any to any 80 in
$ sudo ipfw show
```
