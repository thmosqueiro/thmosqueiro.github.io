---
layout: posts
title: Serial port in Nvidia Jetson board on ubuntu
categories: Signal
tags: arm signal
comments: true
---


The <a
href="http://www.nvidia.com/object/jetson-tk1-embedded-dev-kit.html">Nvidia
Jetson </a> is a powerful  is a powerful board that uses the NVIDIA Tegra® K1
SoC and the same NVIDIA Kepler™ computing core designed. Given the computation
power and the relative low energy, it is a competitve option to developping
embedded systems. We wanted to have one of these boards getting data from the
electronic nose, which allows (i) fast re-allocation and (ii) good performance
in online computation. The electronic nose uses a FTDI chip to stream the
recordings through the USB serial port, which in linux is usually automatically
mounted in /dev/ttyUSBX (X being 0 in the abscence of other serial ports).

<img src="/files/posts/nvidia_jetson.jpeg"
alt="Picture of the Nvidia-Jetson" class='post-img' style="width: 60%;" />

<!-- Split Here - Snapshot -->

I am using the Ubuntu version available from Linux For Tegra (R24.2.1)
maintained by Nvidia. Once the FTDI chip is plugged in, it is visible:

```
user@nose:~$ lsusb Bus 002 Device
002: ID 0403:6001 Future Technology Devices International, Ltd FT232 Serial (UART) IC
```

But it will not mount as serial port:

```
user@nose:~$ dmesg | grep tty
[    0.000000] console [tty1] enabled
[    1.822624] serial8250.0: ttyS0 at MMIO 0x70006300 (irq = 122) is a Tegra
[    4.148550] console [ttyS0] enabled
[    4.154460] serial-tegra.0: ttyTHS0 at MMIO 0x70006000 (irq = 68) is a SERIAL_TEGRA
[    4.166339] serial-tegra.1: ttyTHS1 at MMIO 0x70006040 (irq = 69) is a SERIAL_TEGRA
[    4.178319] serial-tegra.2: ttyTHS2 at MMIO 0x70006200 (irq = 78) is a SERIAL_TEGRA
```

After checking for drivers, you will notice that drivers for the FTDI converter
device are not set by default.

```
user@nose:~$ zcat /proc/config.gz | grep FTDI
# CONFIG_USB_SERIAL_FTDI_SIO is not set
# CONFIG_USB_FTDI_ELAN is not set
```

Thus, to solve this we will need to compile it from the source.

## Compiling FTDI module

Downloading the kernel source and uncompressing:
```
wget http://developer.download.nvidia.com/embedded/L4T/r21_Release_v4.0/source/kernel_src.tbz2
bzip2 -d kernel_src.tbz2
tar -xvf kernel_src.tar
```
Make sure you get the right kernel for you. Mine was version 3.10.40... First time I got it wrong!

Copying the current kernel config into the folder and running menuconfig:
```
zcat /proc/config.gz > ~/kernel/.config
cd kernel
make menuconfig
```

Then navigate to:
```
Device Drivers -> USB Support -> USB Serial Converter Support
```

Mark "M" for Module in the option ```USB FTDI Single Port Serial Driver```. Save it, but do not exit menyconfig yet! Now it is important to check if the ```CONFIG_LOCALVERSION``` variable is matching your kernel. If you don't check this, you may see a message like this:

```
[   36.225047] ftdi_sio: version magic '3.10.40 SMP preempt mod_unload ARMv7 p2v8 ' should be '3.10.40-gdacac96 SMP preempt mod_unload ARMv7 p2v8 '
```

To check if your kernel has a ```CONFIG_LOCALVERSION``` set, use the uname command.

```
user@nose:~$:/home/ubuntu/kernel# uname -a
Linux tegra-ubuntu 3.10.40-gdacac96 #1 SMP PREEMPT Thu Jun 25 15:25:11 PDT 2015 armv7l armv7l armv7l GNU/Linux
```

In the case above, the ```CONFIG_LOCALVERSION``` was set to ```-gdacac96```. We
will next append it to the kernel version DURING the compilation of our module.
Fortunately, this can be done with menuconfig. Go to the original menu, and
select ```General Setup->Local Version```. A window will open where you can set
up the local version, which will be translated into the variable
```CONFIG_LOCALVERSION```. Just type in the local conversion, which is what
comes after the dase (with dash included!). So, in the case above, I typed
"-gdacac96".

<br>
Save again and exit the menuconfig. Finally, we build the modules (read the make!):

```
make prepare
make modules_prepare
make M=drivers/usb/serial/

# copying back the drivers
sudo cp drivers/usb/serial/ftdi_sio.ko /lib/modules/$(uname -r)/kernel
sudo depmod -a
```

No errors mean everything went smoothly, and you should have the drivers
available already.


## Conclusion and testing the final setup

After that, I rebooted the Jetson board and tried dmesg again:

```
user@nose:~$ dmesg | grep tty
...
[ 1066.441572] usb 2-1: FTDI USB Serial Device converter now attached to ttyUSB0
```

If you have problems with permission for reading/writing to the serial port, try adding
your user to the dialout group.

```
usermod -a -G dialout user
```

I also noticed that the electronic nose (picture below) has to be hooked up when
the operating system boots, otherwise your user may not have proper permissions
(even after adding it to dialout group). This has to be related to when the
device is mounted, although I have not figured out exactly why.

<img src="/files/posts/enose.png"
alt="Picture of the Nvidia-Jetson" class='post-img' style="width:90%;" />

After all this, the serial port works perfectly, and its performance is more
than reasonable even when using simple parallel processing ([click here for the
software](https://github.com/VandroiyLabs/FaroresWind)).
