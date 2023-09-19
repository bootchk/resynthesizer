The file vagga.yaml is a vagga script for testing the build process of resynthesizer.
It builds resynthesizer in a disposable container (a VM like Docker)
Vagga builds as a user, without requiring root privileges.
You can modify the script to easily test building on many versions of Linux and GIMP,
without affecting your machine.

Vagga is a tool to create development environments in containers.
To use this script, you first need to install vagga.
See https://vagga.readthedocs.io/en/latest/what_is_vagga.html.
Vagga uses Linux; using vagga on Windows and OSX is inside a Docker VM.

Then:
    >mkdir <buildResynth>
    copy this script to the directory
    >cd <buildResynth>
    clone gimp and resyntheizer repos into the directory
    >git clone ... some Gitlab URI...
    >git clone https://github.com/bootchk/resynthesizer.git
    list remote branches and track/checkout a resynthesizer branch
    >git branch -a
    >git checkout -b resynthesizer3
    >vagga   (will list commands offered by this script.  Expect e.g. "run  runHeal")
    >vagga vagga --use-env DISPLAY run
     (tells vagga to run the command "run".
     use-env tells it to display in a window from your desktop, running outside the container.)

The first time you do this, it will take a long time, building containers by downloading many packages and compiling.

At the end, expect the Gimp app to run (in the container but using your display)
and for the resynthesizer plugin to be in Gimp.

Subsequently, you can modify the script and again >vagga listResynth.
Whenever the script is modified, vagga will check whether to rebuild containers
and whether to rebuild Gimp and may seem to hang, so be patient.

You can and should delete the containers.
The containers are stored completely in the large, hidden directory .vagga in the current directory, owned by you.
Just delete that directory, usually using >sudo rm -rf .vagga (be careful)

The scripts set up a container that contains all the dependencies for Gimp.
The container has a newly built gegl and babl installed.
The container doesn't have Gimp installed.
The vagga commands build Gimp and resynthesizer and install them temporarily.
(vagga calls it transient.)

You can also edit the Gimp or resynthesizer source.
The next time you run a command, the command does a meson rebuild.
Usually it is quick, due to the magic of meson only building what needs to be built.

To change how the resynthesizer (or Gimp) build is configured,
change the -D lines in the commands section of the script.
Thus you can easily build many different configurations of Gimp and Resynthesizer.

Reiterating, the script has "container" sections where you change OS and dependencies.
And the script has "command" sections where you change the build configuration of Gimp or resynthesizer.

I used this script to test building on
Ubuntu 18.10 (cosmic) with the GIMP 2.10.6 packaged for that distribution,
and on Ubuntu 19.04 (disco) with GIMP 2.10.8,
while my machine was still on 16.04 and GIMP 2.8.
To test building resynthesizer on the next version of Ubuntu,
I will simply need to change one word in the script: disco => eoan.
I think that you could similarly test that resynthesizer will build on many recent Linux distributions
(by changing the baseOS container.)

The script is readable and documents (again) the basic requirements to build resynthesizer
(which have not changed recently.)
The scripts illustrate the rats nest of dependencies, including required tools and libraries.
For example, you can build resynthesizer without installing GIMP or Python,
but resynthesizer won't install or run, i.e. GIMP and Python are runtime dependencies.


