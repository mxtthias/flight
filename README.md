Flight - A flocking simulation written in Erlang
================================================

Purpose
-------

This is first and foremost an experiment with using (and abusing) OTP,
especially gen_event.


Dependencies
------------

Erlang (only tested on R16B02) and rebar. The Makefile assumes rebar
is in your $PATH.


Running
-------

```
$ cd flight
$ make && make test
$ erl -pa ebin -s flight_app start

1> bird_sup:add_birds().
```

If you want to add more individuals, you can use bird_sup:add_birds/2
or bird_sup:add_bird/0 as well.

Since the processes don't output anything to the screen, this will
feel quite pointless until you add an event handler and visualize the
messages.


'Protocol'
----------

There are two types of events: introduce and move. The messages have
the following format:

```
{introduce, Pid, Position, Direction}
{move, Pid, From, To}
```


Design
------

The idea is that by having each individual follow three simple rules,
you will get something that behaves in the same way as a flock of
birds. The rules are as follows:

* Don't fly into anything ('walls', obstacles or other individuals)
* Fly in the same direction as those close to you
* Keep the whole flock together

The individuals are implemented as a gen_server and a gen_event, used
for communication. Every action triggers an event that is broadcasted
to all the registered event handlers. Broadcasts isn't really the
right way to implement this, but I'm more interested in trying out
gen_event than in doing things right.


TODO
----

- [ ] Add flock 'gravitation'
- [ ] Handle individuals bumping into each other

