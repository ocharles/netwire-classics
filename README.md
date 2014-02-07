netwire-classics
================

Classic games recreated in netwire

### Download and Install Nix

https://www.domenkozar.com/2014/01/02/getting-started-with-nix-package-manager/
https://gist.github.com/iElectric/8217950 - Nix install script

### Alternatively install NixOS

http://nixos.org/nix/download.html

#####Install nix
```bash
#subscribe to nixpkgs-unstable channel
nix-channel --add http://nixos.org/channels/nixpkgs-unstable
#download current nix expressions
nix-channel --update
#update packages of user profile from downoaded nix expressions
nix-env -u '*'
```

###Compile netwire-classics using Nix

To compile add required LICENSE file to asteroids folder.
Then in the netwire-classics folder run:
```bash
nix-build
```
