# ConfSnap
Yet another (network device) configuration backup utility.

Why ? Because existing solutions, like RANCID and Oxidized, feel overly complex for a simple task such as device configuration archiving.

This is a simple tool that does the fetching, displaying and diffing configuration snapshots. You can invoke it manually from shell, from cron, from another program/script ... however you like.
If you want to snapshot device's configuration as soon as change has been made, you can catch change events with just about any modern syslog daemon (rsyslog, syslog-ng, nxlog, ...) and invoke ConfSnap as external command or, if you use, say, Graylog, you can trigger it via alarm callbacks.

Due to it's modular architecture, it can be easily extended to support just about any device/OS that you can remotely log in to and retrieve it's configuration.

* Supports indirect configuration fetch (for example - from managed CPEs connected to VRFs on your PE routers) by logging into a network device and then logging in from there to the target device.
* Uses GIT to keep track of configuration change history and supports synchronizing with remote GIT repos.
* Allows you to review any configuration snapshot from history or diff between 2 chosen configuration snapshots.
