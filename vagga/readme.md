## About

This directory has scripts for building resynthesizer in a disposable container.
That is, build without affecting your personal computer, the "host" of the "container."

A file vagga.yaml is a vagga script for testing the build process of resynthesizer.
It builds resynthesizer in a disposable container (a VM like Docker.)

Vagga builds as a user, without requiring root privileges.

You can modify the script to easily test building on many versions of Linux and GIMP,
without affecting your machine.

## Requires the tool "vagga"

Using these scripts does require you to install the tool "vagga" in your personal computer.

Vagga is a tool to create development environments in containers.
To use this script, you first need to install vagga.
See https://vagga.readthedocs.io/en/latest/what_is_vagga.html.
Vagga uses Linux; using vagga on Windows and OSX is inside a Docker VM.

## Using the scripts

```
    # make a "work" directory
    >mkdir <buildResynth>
    # copy this script into the work directory
    >cd <buildResynth>
    # clone gimp repo into the work directory
    >git clone ... some Gitlab URI...
    # clone resynthesizer repo 
    >git clone https://github.com/bootchk/resynthesizer.git
    # list remote branches
    >git branch -a
    # track/checkout a resynthesizer branch
    >git checkout -b resynthesizer3
    # list vagga commands offered by this script.
    >vagga
    # execute the vagga "run" command
    # This is for X11 window manager
    >vagga --use-env DISPLAY run
    # tells vagga to run the command "run".
    # use-env tells it to display in a window from the host,
    # your personal computer.
```

The first time you do this, it will take a long time, building containers by downloading many packages and compiling.

At the end, expect the GIMP app to run 
(in the container but using the display of the host)
and for the resynthesizer plugin to be in GIMP.

## Modifying the vagga.yaml script

Subsequently, you can modify the script vagga.yaml 
and again >vagga run.
Whenever the script is modified, 
vagga will check whether to rebuild containers
and whether to rebuild GIMP.
The tool may seem to hang, so be patient.

## Deleting the containers

You can and should delete the containers.
The containers are stored completely in the large, hidden directory \
".vagga" in the current directory, owned by you.
Just delete that directory, 
usually using >sudo rm -rf .vagga (be careful)

## The structure of the containers

The vagga scripts set up a container that contains all the dependencies for GIMP.
The container has a newly built gegl and babl installed.
The container doesn't have GIMP installed.
The vagga commands such as "vagga run"
build GIMP and resynthesizer and install them temporarily.
(vagga calls it transient.)

There are actually many containers, nested.
The vagga tool knows when to rebuild containers
when you edit the vagga.yaml script
(for example, changing a declared dependency of GIMP.)

## Developing by editing GIMP and resynthesizer source

You can also edit the GIMP or resynthesizer source.
The next time you run a command, the command does a meson rebuild.
Usually it is quick, due to the magic of meson only building what needs to be built.

To change how the resynthesizer (or GIMP) build is configured,
change the -D lines in the commands section of the script.
Thus you can easily build many different configurations of GIMP and Resynthesizer.

Reiterating, the script has "container" sections where you change OS and dependencies.
And the script has "command" sections where you change the build configuration of GIMP or resynthesizer.

## History of these scripts

I used this script to test building on:


   - Ubuntu 18.10 (cosmic) with the GIMP 2.10.6 packaged for that distribution,
   - Ubuntu 19.04 (disco) with GIMP 2.10.8
while my machine was still on 16.04 and GIMP 2.8.
   - Ubuntu 24.04 (noble) GIMP 3 on host Ubuntu 2.04 (2025)

To test building resynthesizer on the next version of Ubuntu,
I will simply need to change a few lines in the script.
I think that you could similarly test that resynthesizer will build on many recent Linux distributions
(by changing the baseOS container.)

## The vagga script as a document how to build GIMP and Resynthesizer

The script is readable and documents (again) the basic requirements to build resynthesizer
(which have not changed recently.)
The scripts illustrate the rats nest of dependencies, including required tools and libraries.

For example, you can build resynthesizer without installing GIMP or Python,
(but you do need the GIMP library and headers installed)
but resynthesizer won't install or run,
because GIMP and Python are runtime dependencies.

## Permit the container to use the host display

When the script runs GIMP, GIMP wants to open windows in a "display",
and the display manager is running in the host outside the container
where the GIMP app is running.

### X11

Pass the env var "DISPLAY" to the vagga command.
```
vagga --use-env DISPLAY run
```

### Wayland

Give permissions to access the display socket
```
xhost + local:
```

Define a vagga external volume in your ~/.config/vagga/settings.yaml
```
# wayland
external-volumes:
  runuser: /run/user/1000
```

Mount /run to that external volume in the declaration of the container,
in vagga.yaml:
```
volumes:
      # bind external volume named runuser pointing to external /run/user/1000 to /run in container
      /run: !BindRW /volumes/runuser
```

In the declaration of a vagga command, in vagga.yaml:
```
export XDG_RUNTIME_DIR=/run
```
or if it is defined in your env, pass it into the container:
```
vagga --use-env XDG_RUNTIME_DIR run
```
