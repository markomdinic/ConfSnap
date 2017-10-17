# ConfSnap
Yet another (network device) configuration backup utility.

Due to it's modular architecture, it can be easily extended to support just about any device/OS that you can remotely log in to and retrieve it's configuration.

* Supports indirect configuration fetch (for example - from managed CPEs connected to VRFs on your PE routers) by logging into a network device and then logging in from there to the target device.
* Uses GIT to keep track of configuration change history and supports synchronizing with remote GIT repos.
* Allows you to review current device configuration or diff between 2 chosen configuration snapshots.
