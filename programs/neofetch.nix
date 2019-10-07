{ pkgs, lib, ... }:
let
  host = pkgs.callPackage ../hosts {};
in
{
  # home.packages = lib.optionals (!host.is.NixOS) [ pkgs.neofetch ];
  home.packages = [ pkgs.neofetch ];
  xdg.configFile."neofetch/config.conf".text = pkgs.lib.strings.concatStringsSep "\n" [
    # see this wiki page for more info:
    # https://github.com/dylanaraps/neofetch/wiki/customizing-info
    ''
    print_info() {
        info title
        info underline

        info "os" distro
        info "host" model
        info "kernel" kernel
        info "uptime" uptime
        # info "packages" packages
        info "shell" shell
        info "resolution" resolution
        info "de" de
        info "wm" wm
        info "wm theme" wm_theme
        info "theme" theme
        info "icons" icons
        info "terminal" term
        info "terminal font" term_font
        info "cpu" cpu
        info "gpu" gpu
        info "memory" memory

        info "gpu driver" gpu_driver  # linux/macos only
        info "cpu usage" cpu_usage
        info "disk" disk
        # info "battery" battery
        info "font" font
        # info "song" song
        # [[ $player ]] && prin "music player" "$player"
        info "local ip" local_ip
        # info "public ip" public_ip
        # info "users" users
        info "locale" locale  # this only works on glibc systems.

        info cols
    }
    ''

    # kernel

    # shorten the output of the kernel function.
    #
    # default:  'on'
    # values:   'on', 'off'
    # flag:     --kernel_shorthand
    # supports: everything except *bsds (except pacbsd and pc-bsd)
    #
    # example:
    # on:  '4.8.9-1-arch'
    # off: 'linux 4.8.9-1-arch'
    ''kernel_shorthand="on"''


    # distro


    # shorten the output of the distro function
    #
    # default:  'off'
    # values:   'on', 'off', 'tiny'
    # flag:     --distro_shorthand
    # supports: everything except windows and haiku
    ''distro_shorthand="off"''

    # show/hide os architecture.
    # show 'x86_64', 'x86' and etc in 'distro:' output.
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --os_arch
    #
    # example:
    # on:  'arch linux x86_64'
    # off: 'arch linux'
    ''os_arch="on"''


    # uptime


    # shorten the output of the uptime function
    #
    # default: 'on'
    # values:  'on', 'off', 'tiny'
    # flag:    --uptime_shorthand
    #
    # example:
    # on:   '2 days, 10 hours, 3 mins'
    # off:  '2 days, 10 hours, 3 minutes'
    # tiny: '2d 10h 3m'
    ''uptime_shorthand="on"''


    # memory


    # show memory pecentage in output.
    #
    # default: 'off'
    # values:  'on', 'off'
    # flag:    --memory_percent
    #
    # example:
    # on:   '1801mib / 7881mib (22%)'
    # off:  '1801mib / 7881mib'
    ''memory_percent="off"''


    # packages


    # show/hide package manager names.
    #
    # default: 'tiny'
    # values:  'on', 'tiny' 'off'
    # flag:    --package_managers
    #
    # example:
    # on:   '998 (pacman), 8 (flatpak), 4 (snap)'
    # tiny: '908 (pacman, flatpak, snap)'
    # off:  '908'
    ''package_managers="on"''


    # shell


    # show the path to $shell
    #
    # default: 'off'
    # values:  'on', 'off'
    # flag:    --shell_path
    #
    # example:
    # on:  '/bin/bash'
    # off: 'bash'
    ''shell_path="off"''

    # show $shell version
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --shell_version
    #
    # example:
    # on:  'bash 4.4.5'
    # off: 'bash'
    ''shell_version="on"''


    # cpu


    # cpu speed type
    #
    # default: 'bios_limit'
    # values: 'scaling_cur_freq', 'scaling_min_freq', 'scaling_max_freq', 'bios_limit'.
    # flag:    --speed_type
    # supports: linux with 'cpufreq'
    # note: any file in '/sys/devices/system/cpu/cpu0/cpufreq' can be used as a value.
    ''speed_type="bios_limit"''

    # cpu speed shorthand
    #
    # default: 'off'
    # values: 'on', 'off'.
    # flag:    --speed_shorthand
    # note: this flag is not supported in systems with cpu speed less than 1 ghz
    #
    # example:
    # on:    'i7-6500u (4) @ 3.1ghz'
    # off:   'i7-6500u (4) @ 3.100ghz'
    ''speed_shorthand="off"''

    # enable/disable cpu brand in output.
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --cpu_brand
    #
    # example:
    # on:   'intel i7-6500u'
    # off:  'i7-6500u (4)'
    ''cpu_brand="on"''

    # cpu speed
    # hide/show cpu speed.
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --cpu_speed
    #
    # example:
    # on:  'intel i7-6500u (4) @ 3.1ghz'
    # off: 'intel i7-6500u (4)'
    ''cpu_speed="on"''

    # cpu cores
    # display cpu cores in output
    #
    # default: 'logical'
    # values:  'logical', 'physical', 'off'
    # flag:    --cpu_cores
    # support: 'physical' doesn't work on bsd.
    #
    # example:
    # logical:  'intel i7-6500u (4) @ 3.1ghz' (all virtual cores)
    # physical: 'intel i7-6500u (2) @ 3.1ghz' (all physical cores)
    # off:      'intel i7-6500u @ 3.1ghz'
    ''cpu_cores="logical"''

    # cpu temperature
    # hide/show cpu temperature.
    # note the temperature is added to the regular cpu function.
    #
    # default: 'off'
    # values:  'c', 'f', 'off'
    # flag:    --cpu_temp
    # supports: linux, bsd
    # note: for freebsd and netbsd-based systems, you'll need to enable
    #       coretemp kernel module. this only supports newer intel processors.
    #
    # example:
    # c:   'intel i7-6500u (4) @ 3.1ghz [27.2°c]'
    # f:   'intel i7-6500u (4) @ 3.1ghz [82.0°f]'
    # off: 'intel i7-6500u (4) @ 3.1ghz'
    ''cpu_temp="off"''


    # gpu


    # enable/disable gpu brand
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --gpu_brand
    #
    # example:
    # on:  'amd hd 7950'
    # off: 'hd 7950'
    ''gpu_brand="on"''

    # which gpu to display
    #
    # default: 'all'
    # values:  'all', 'dedicated', 'integrated'
    # flag:    --gpu_type
    # supports: linux
    #
    # example:
    # all:
    #   gpu1: amd hd 7950
    #   gpu2: intel integrated graphics
    #
    # dedicated:
    #   gpu1: amd hd 7950
    #
    # integrated:
    #   gpu1: intel integrated graphics
    ''gpu_type="all"''


    # resolution


    # display refresh rate next to each monitor
    # default: 'off'
    # values:  'on', 'off'
    # flag:    --refresh_rate
    # supports: doesn't work on windows.
    #
    # example:
    # on:  '1920x1080 @ 60hz'
    # off: '1920x1080'
    ''refresh_rate="on"''


    # gtk theme / icons / font


    # shorten output of gtk theme / icons / font
    #
    # default: 'off'
    # values:  'on', 'off'
    # flag:    --gtk_shorthand
    #
    # example:
    # on:  'numix, adwaita'
    # off: 'numix [gtk2], adwaita [gtk3]'
    ''gtk_shorthand="off"''


    # enable/disable gtk2 theme / icons / font
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --gtk2
    #
    # example:
    # on:  'numix [gtk2], adwaita [gtk3]'
    # off: 'adwaita [gtk3]'
    ''gtk2="on"''

    # enable/disable gtk3 theme / icons / font
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --gtk3
    #
    # example:
    # on:  'numix [gtk2], adwaita [gtk3]'
    # off: 'numix [gtk2]'
    ''gtk3="on"''


    # ip address


    # website to ping for the public ip
    #
    # default: 'http://ident.me'
    # values:  'url'
    # flag:    --ip_host
    ''public_ip_host="http://ident.me"''

    # public ip timeout.
    #
    # default: '2'
    # values:  'int'
    # flag:    --ip_timeout
    ''public_ip_timeout=2''


    # disk


    # which disks to display.
    # the values can be any /dev/sdxx, mount point or directory.
    # note: by default we only show the disk info for '/'.
    #
    # default: '/'
    # values:  '/', '/dev/sdxx', '/path/to/drive'.
    # flag:    --disk_show
    #
    # example:
    # disk_show=('/' '/dev/sdb1'):
    #      'disk (/): 74g / 118g (66%)'
    #      'disk (/mnt/videos): 823g / 893g (93%)'
    #
    # disk_show=('/'):
    #      'disk (/): 74g / 118g (66%)'
    #
    ''disk_show=('/', '/home/stites', '/boot')''

    # disk subtitle.
    # what to append to the disk subtitle.
    #
    # default: 'mount'
    # values:  'mount', 'name', 'dir'
    # flag:    --disk_subtitle
    #
    # example:
    # name:   'disk (/dev/sda1): 74g / 118g (66%)'
    #         'disk (/dev/sdb2): 74g / 118g (66%)'
    #
    # mount:  'disk (/): 74g / 118g (66%)'
    #         'disk (/mnt/local disk): 74g / 118g (66%)'
    #         'disk (/mnt/videos): 74g / 118g (66%)'
    #
    # dir:    'disk (/): 74g / 118g (66%)'
    #         'disk (local disk): 74g / 118g (66%)'
    #         'disk (videos): 74g / 118g (66%)'
    ''disk_subtitle="mount"''


    # song


    # manually specify a music player.
    #
    # default: 'auto'
    # values:  'auto', 'player-name'
    # flag:    --music_player
    #
    # available values for 'player-name':
    #
    # amarok
    # audacious
    # banshee
    # bluemindo
    # clementine
    # cmus
    # deadbeef
    # deepin-music
    # dragon
    # elisa
    # exaile
    # gnome-music
    # gmusicbrowser
    # guayadeque
    # itunes
    # juk
    # lollypop
    # mocp
    # mopidy
    # mpd
    # netease-cloud-music
    # pogo
    # pragha
    # qmmp
    # quodlibet
    # rhythmbox
    # sayonara
    # smplayer
    # spotify
    # tomahawk
    # vlc
    # xmms2d
    # yarock
    ''music_player="auto"''

    # format to display song information.
    #
    # default: '%artist% - %album% - %title%'
    # values:  '%artist%', '%album%', '%title%'
    # flag:    --song_format
    #
    # example:
    # default: 'song: jet - get born - sgt major'
    ''song_format="%artist% - %album% - %title%"''

    # print the artist, album and title on separate lines
    #
    # default: 'off'
    # values:  'on', 'off'
    # flag:    --song_shorthand
    #
    # example:
    # on:  'artist: the fratellis'
    #      'album: costello music'
    #      'song: chelsea dagger'
    #
    # off: 'song: the fratellis - costello music - chelsea dagger'
    ''song_shorthand="off"''

    # 'mpc' arguments (specify a host, password etc).
    #
    # default:  ''
    # example: mpc_args=(-h host -p password)
    ''mpc_args=()''


    # text colors


    # text colors
    #
    # default:  'distro'
    # values:   'distro', 'num' 'num' 'num' 'num' 'num' 'num'
    # flag:     --colors
    #
    # each number represents a different part of the text in
    # this order: 'title', '@', 'underline', 'subtitle', 'colon', 'info'
    #
    # example:
    # colors=(distro)      - text is colored based on distro colors.
    # colors=(4 6 1 8 8 6) - text is colored in the order above.
    ''colors=(distro)''


    # text options


    # toggle bold text
    #
    # default:  'on'
    # values:   'on', 'off'
    # flag:     --bold
    ''bold="on"''

    # enable/disable underline
    #
    # default:  'on'
    # values:   'on', 'off'
    # flag:     --underline
    ''underline_enabled="on"''

    # underline character
    #
    # default:  '-'
    # values:   'string'
    # flag:     --underline_char
    ''underline_char="-"''


    # info separator
    # replace the default separator with the specified string.
    #
    # default:  ':'
    # flag:     --separator
    #
    # example:
    # separator="->":   'shell-> bash'
    # separator=" =":   'wm = dwm'
    ''separator=":"''


    # color blocks


    # color block range
    # the range of colors to print.
    #
    # default:  '0', '7'
    # values:   'num'
    # flag:     --block_range
    #
    # example:
    #
    # display colors 0-7 in the blocks.  (8 colors)
    # neofetch --block_range 0 7
    #
    # display colors 0-15 in the blocks. (16 colors)
    # neofetch --block_range 0 15
    ''block_range=(0 7)''

    # toggle color blocks
    #
    # default:  'on'
    # values:   'on', 'off'
    # flag:     --color_blocks
    ''color_blocks="on"''

    # color block width in spaces
    #
    # default:  '3'
    # values:   'num'
    # flag:     --block_width
    ''block_width=3''

    # color block height in lines
    #
    # default:  '1'
    # values:   'num'
    # flag:     --block_height
    ''block_height=1''


    # progress bars


    # bar characters
    #
    # default:  '-', '='
    # values:   'string', 'string'
    # flag:     --bar_char
    #
    # example:
    # neofetch --bar_char 'elapsed' 'total'
    # neofetch --bar_char '-' '='
    ''bar_char_elapsed="-"''
    ''bar_char_total="="''

    # toggle bar border
    #
    # default:  'on'
    # values:   'on', 'off'
    # flag:     --bar_border
    ''bar_border="on"''

    # progress bar length in spaces
    # number of chars long to make the progress bars.
    #
    # default:  '15'
    # values:   'num'
    # flag:     --bar_length
    ''bar_length=15''

    # progress bar colors
    # when set to distro, uses your distro's logo colors.
    #
    # default:  'distro', 'distro'
    # values:   'distro', 'num'
    # flag:     --bar_colors
    #
    # example:
    # neofetch --bar_colors 3 4
    # neofetch --bar_colors distro 5
    ''bar_color_elapsed="distro"''
    ''bar_color_total="distro"''


    # info display
    # display a bar with the info.
    #
    # default: 'off'
    # values:  'bar', 'infobar', 'barinfo', 'off'
    # flags:   --cpu_display
    #          --memory_display
    #          --battery_display
    #          --disk_display
    #
    # example:
    # bar:     '[---=======]'
    # infobar: 'info [---=======]'
    # barinfo: '[---=======] info'
    # off:     'info'
    ''cpu_display="off"''
    ''memory_display="off"''
    ''battery_display="off"''
    ''disk_display="off"''


    # backend settings


    # image backend.
    #
    # default:  'ascii'
    # values:   'ascii', 'caca', 'chafa', 'jp2a', 'iterm2', 'off',
    #           'termpix', 'pixterm', 'tycat', 'w3m', 'kitty'
    # flag:     --backend
    ''image_backend="kitty"''

    # image source
    #
    # which image or ascii file to display.
    #
    # default:  'auto'
    # values:   'auto', 'ascii', 'wallpaper', '/path/to/img', '/path/to/ascii', '/path/to/dir/'
    #           'command output (neofetch --ascii "$(fortune | cowsay -w 30)")'
    # flag:     --source
    #
    # note: 'auto' will pick the best image source for whatever image backend is used.
    #       in ascii mode, distro ascii art will be used and in an image mode, your
    #       wallpaper will be used.
    ''image_source="auto"''


    # ascii options


    # ascii distro
    # which distro's ascii art to display.
    #
    # default: 'auto'
    # values:  'auto', 'distro_name'
    # flag:    --ascii_distro
    #
    # note: arch and ubuntu have 'old' logo variants.
    #       change this to 'arch_old' or 'ubuntu_old' to use the old logos.
    # note: ubuntu has flavor variants.
    #       change this to 'lubuntu', 'xubuntu', 'ubuntu-gnome' or 'ubuntu-budgie' to use the flavors.
    # note: arch, crux and gentoo have a smaller logo variant.
    #       change this to 'arch_small', 'crux_small' or 'gentoo_small' to use the small logos.
    ''ascii_distro="auto"''

    # ascii colors
    #
    # default:  'distro'
    # values:   'distro', 'num' 'num' 'num' 'num' 'num' 'num'
    # flag:     --ascii_colors
    #
    # example:
    # ascii_colors=(distro)      - ascii is colored based on distro colors.
    # ascii_colors=(4 6 1 8 8 6) - ascii is colored using these colors.
    ''ascii_colors=(distro)''

    # bold ascii logo
    # whether or not to bold the ascii logo.
    #
    # default: 'on'
    # values:  'on', 'off'
    # flag:    --ascii_bold
    ''ascii_bold="on"''


    # image options


    # image loop
    # setting this to on will make neofetch redraw the image constantly until
    # ctrl+c is pressed. this fixes display issues in some terminal emulators.
    #
    # default:  'off'
    # values:   'on', 'off'
    # flag:     --loop
    ''image_loop="off"''

    # thumbnail directory
    #
    # default: '~/.cache/thumbnails/neofetch'
    # values:  'dir'
    ''thumbnail_dir="''${xdg_cache_home:-''${home}/.cache}/thumbnails/neofetch"''

    # crop mode
    #
    # default:  'normal'
    # values:   'normal', 'fit', 'fill'
    # flag:     --crop_mode
    #
    # see this wiki page to learn about the fit and fill options.
    # https://github.com/dylanaraps/neofetch/wiki/what-is-waifu-crop%3f
    ''crop_mode="normal"''

    # crop offset
    # note: only affects 'normal' crop mode.
    #
    # default:  'center'
    # values:   'northwest', 'north', 'northeast', 'west', 'center'
    #           'east', 'southwest', 'south', 'southeast'
    # flag:     --crop_offset
    ''crop_offset="center"''

    # image size
    # the image is half the terminal width by default.
    #
    # default: 'auto'
    # values:  'auto', '00px', '00%', 'none'
    # flags:   --image_size
    #          --size
    ''image_size="auto"''

    # gap between image and text
    #
    # default: '3'
    # values:  'num', '-num'
    # flag:    --gap
    ''gap=3''

    # image offsets
    # only works with the w3m backend.
    #
    # default: '0'
    # values:  'px'
    # flags:   --xoffset
    #          --yoffset
    ''yoffset=0''
    ''xoffset=0''

    # image background color
    # only works with the w3m backend.
    #
    # default: ''
    # values:  'color', 'blue'
    # flag:    --bg_color
    ''background_color=''


    # misc options

    # stdout mode
    # turn off all colors and disables image backend (ascii/image).
    # useful for piping into another command.
    # default: 'off'
    # values: 'on', 'off'
    ''stdout="off"''
    ];

}
