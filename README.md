Flight - A flocking simulation written in Erlang
================================================

Purpose
-------

This is first and foremost an experiment with using (and abusing) OTP,
especially gen_event.


Design
------

Each individual is modelled as a separate process and will adhere to
the following rules (in order of precedence):

* Don't fly into anything ('walls' or other individuals)
* Fly in the same direction as those closes to you
* Don't split the group

