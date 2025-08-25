# 🖥️ EXWM Desktop Environment - Emacs AI 2.0

[![EXWM](https://img.shields.io/badge/Window%20Manager-EXWM-green.svg)](https://github.com/ch11ng/exwm)
[![Emacs](https://img.shields.io/badge/Emacs-27.1+-blue.svg)](https://www.gnu.org/software/emacs/)
[![License](https://img.shields.io/badge/License-GPL%203.0-green.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-brightgreen.svg)](https://github.com/krispi/emacsAI_2)

> **🌟 Complete EXWM desktop environment providing seamless integration between Emacs and X11 applications, with professional window management and system integration.**

## ✨ What is EXWM?

EXWM (Emacs X Window Manager) transforms Emacs into a complete window manager, providing:

- **🔄 Seamless Integration**: X11 windows become Emacs buffers
- **🎯 Tiling Management**: Automatic window arrangement and layout
- **⌨️ Emacs Control**: All window operations through Emacs
- **🏠 Workspace Support**: Virtual desktop management
- **🌐 Application Integration**: Native X11 applications work seamlessly

## 🏗️ Architecture

### **Modular Configuration Structure**
```
Exwm/
├── 📄 exwm-config.el           # Main EXWM configuration
├── 📄 elpaca-integration.el    # Package management integration
├── 📄 system-integration.el    # System-level integration
├── 📄 exwm-keybindings.el     # Keybindings and shortcuts
├── 📁 config/                  # System configuration files
│   └── 📁 exwm/
│       ├── 📄 autostart-improved.sh  # Application autostart
│       └── 📄 exwm-powermenu.sh     # Power management menu
├── 📄 emacs.desktop            # LightDM session entry
├── 📄 xinitrc.sh               # X11 startup script
└── 📖 README.md                # This documentation
```

### **Module Loading Order**
1. **exwm-config.el** - Core EXWM configuration
2. **system-integration.el** - System tray, notifications, power management
3. **exwm-keybindings.el** - All keybindings and shortcuts
4. **elpaca-integration.el** - Package management and dependencies

## 🚀 Quick Start

### **System Requirements**
- **Linux distribution** (Arch Linux recommended)
- **Emacs 27.1+** (29.x or 30.x for best experience)
- **X11** (required for EXWM)
- **Git** for package management

### **Installation**

1. **Install dependencies**
   ```bash
   # Arch Linux
   sudo pacman -S emacs exwm xorg-server xorg-xinit rofi picom dunst
   
   # Ubuntu/Debian
   sudo apt install emacs exwm xorg xinit rofi picom dunst
   ```

2. **Clone configuration**
   ```bash
   git clone https://github.com/krispi/emacsAI_2.git ~/.emacs.d
   cd ~/.emacs.d
   ```

3. **Start EXWM**
   ```bash
   # Direct start
   ./Exwm/xinitrc.sh
   
   # Or through LightDM
   sudo cp Exwm/emacs.desktop /usr/share/xsessions/
   sudo systemctl enable lightdm
   # Reboot and select "Emacs + EXWM"
   ```

## 🎯 Key Features

### **🏠 Workspace Management**
- **4 Virtual Workspaces**: Organized by task type
- **Automatic Layout**: Smart window arrangement
- **Easy Navigation**: Super + 1-4 to switch workspaces
- **Persistent Layouts**: Remember window arrangements

### **🎨 Application Management**
- **Rofi Integration**: Modern application launcher
- **Window Switcher**: Quick window navigation
- **Power Menu**: System shutdown, restart, lock
- **Auto-placement**: Smart application positioning

### **🖥️ Window Management**
- **Tiling Layouts**: Automatic window arrangement
- **Floating Windows**: Support for dialog boxes
- **Fullscreen Support**: Toggle fullscreen mode
- **Window Resizing**: Dynamic window sizing

### **🔧 System Integration**
- **System Tray**: Integrated status indicators
- **Notifications**: Dunst notification system
- **Power Management**: Brightness and volume control
- **Multi-monitor**: Full multi-monitor support

## ⌨️ Key Bindings

### **Workspace Management**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + 1-4` | Workspace Switch | Switch to workspace 1-4 |
| `Super + ←/→` | Move Window | Move window between workspaces |
| `Super + ↑/↓` | Resize Window | Resize current window |

### **Application Launchers**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + p` | Rofi Launcher | Launch applications |
| `Super + Tab` | Window Switcher | Switch between windows |
| `Super + x` | Power Menu | System power options |
| `Super + Return` | Terminal | Open terminal |

### **Window Management**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + q` | Close Window | Close current window |
| `Super + f` | Fullscreen | Toggle fullscreen mode |
| `Super + m` | Maximize | Toggle maximize mode |
| `Super + t` | Floating | Toggle floating mode |

### **System Control**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + l` | Lock Screen | Lock the screen |
| `Super + r` | Reload EXWM | Reload configuration |
| `Super + i` | Keyboard Mode | Toggle input mode |
| `Super + s` | Screenshot | Take screenshot |

### **Volume & Brightness**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + =` | Volume Up | Increase volume |
| `Super + -` | Volume Down | Decrease volume |
| `Super + 0` | Mute | Toggle mute |
| `Super + +` | Brightness Up | Increase brightness |
| `Super + _` | Brightness Down | Decrease brightness |

### **Media Control**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + PgUp` | Previous Track | Previous media track |
| `Super + PgDn` | Next Track | Next media track |
| `Super + Home` | Play/Pause | Toggle media playback |
| `Super + End` | Stop | Stop media playback |

### **Emergency Keys**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + F4` | Force Quit | Force quit application |
| `Super + F12` | Restart EXWM | Emergency restart |
| `Super + F1` | Emergency Terminal | Emergency terminal access |

## 🔧 Configuration

### **Customizing Keybindings**
Edit `exwm-keybindings.el` to modify keybindings:

```elisp
;; Add custom application shortcut
(,(kbd "s-c") . (lambda ()
                   (interactive)
                   (start-process-shell-command "calculator" nil "gnome-calculator")))
```

### **Application Auto-placement**
Modify `exwm-config.el` to change workspace assignments:

```elisp
(setq exwm-manage-configurations
      '(
        ;; Custom application placement
        ((equal exwm-class-name "YourApp") workspace 2)
        ((equal exwm-class-name "AnotherApp") floating t)
        ))
```

### **Autostart Applications**
Edit `config/exwm/autostart-improved.sh` to add startup applications:

```bash
# Add your custom applications
if is_installed firefox; then
    firefox &
    log_event "Started Firefox"
fi
```

## 🎨 Visual Customization

### **Compositor (Picom)**
- **Transparency**: Window transparency effects
- **Shadows**: Drop shadows for windows
- **Animations**: Smooth window animations
- **Blur**: Background blur effects

### **Status Bar (Polybar)**
- **System Information**: CPU, memory, network
- **Workspace Display**: Current workspace indicator
- **Application Status**: Active application info
- **System Controls**: Volume, brightness, power

### **Rofi Themes**
- **Application Launcher**: Modern app launcher
- **Window Switcher**: Beautiful window selection
- **Power Menu**: System power options
- **Custom Themes**: Personalize appearance

## 🚀 Advanced Features

### **Multi-Monitor Support**
```elisp
;; Distribute workspaces across monitors
(setq exwm-randr-workspace-output-plist
      '(0 "eDP-1"    ; Workspace 0 on laptop
          1 "HDMI-1"  ; Workspace 1 on external
          2 "eDP-1"   ; Workspace 2 on laptop
          3 "HDMI-1")) ; Workspace 3 on external
```

### **Custom Workspace Layouts**
```elisp
;; Custom workspace behavior
(add-hook 'exwm-workspace-switch-hook
          (lambda ()
            (message "Switched to workspace %d" 
                     (exwm-workspace--position (exwm-workspace--current) 0))))
```

### **Application Rules**
```elisp
;; Advanced application management
(setq exwm-manage-configurations
      '(
        ;; Floating applications
        ((equal exwm-class-name "Pavucontrol") floating t)
        ((equal exwm-class-name "Blueman-manager") floating t)
        
        ;; Specific workspace placement
        ((equal exwm-class-name "Firefox") workspace 1)
        ((equal exwm-class-name "Emacs") workspace 3)
        ))
```

## 🔍 Troubleshooting

### **Common Issues**

#### **EXWM not starting**
```bash
# Check X11 is running
echo $DISPLAY

# Check Emacs installation
emacs --version

# Check EXWM package
ls ~/.emacs.d/elpaca/
```

#### **Keybindings not working**
```elisp
;; Reload configuration
M-x exwm-reset

;; Check keybinding setup
M-x exwm-show-keybindings-help
```

#### **Applications not appearing**
```bash
# Check autostart script
~/.config/exwm/autostart.sh

# Check logs
tail -f ~/.config/exwm/autostart.log
```

### **Debug Mode**
Enable debug mode in `exwm-config.el`:

```elisp
;; Enable debug mode
(setq exwm-debug t)

;; Show debug messages
(message "EXWM debug mode enabled")
```

## 📚 Documentation

### **Configuration Files**
- **exwm-config.el**: Core EXWM configuration
- **system-integration.el**: System integration features
- **exwm-keybindings.el**: All keybindings and shortcuts
- **elpaca-integration.el**: Package management

### **Scripts**
- **autostart-improved.sh**: Application startup script
- **exwm-powermenu.sh**: Power management menu
- **xinitrc.sh**: X11 startup script

### **System Integration**
- **emacs.desktop**: LightDM session configuration
- **picom-config.txt**: Compositor configuration
- **rofi-powermenu.txt**: Rofi power menu setup

## 🤝 Contributing

### **Guidelines**
1. **Maintain modularity**: Keep changes isolated to specific modules
2. **Follow standards**: Use professional coding practices
3. **Document changes**: Update documentation for all modifications
4. **Test thoroughly**: Ensure changes don't break existing functionality

### **Development**
- **Fork the repository**
- **Create feature branch**
- **Make your changes**
- **Submit pull request**

## 📄 License

This project is licensed under the **GNU General Public License v3.0** - see the [LICENSE](../LICENSE) file for details.

## 🙏 Acknowledgments

- **EXWM Team**: For the excellent window manager
- **Emacs Community**: For the amazing ecosystem
- **Rofi Team**: For the beautiful application launcher
- **Contributors**: For helping improve this configuration

## 📞 Support

### **Issues**
- **GitHub Issues**: Report bugs and request features
- **Documentation**: Check this README first
- **Community**: Join EXWM community discussions

### **Contact**
- **GitHub**: [krispi/emacsAI_2](https://github.com/krispi/emacsAI_2)
- **Issues**: [GitHub Issues](https://github.com/krispi/emacsAI_2/issues)

---

## 🎉 **Ready for the Ultimate EXWM Experience?**

**Transform your Emacs into a complete desktop environment with professional window management and seamless system integration!**

**🚀 Install • 🎯 Configure • ✨ Enjoy • 🖥️ Transform**

**🎨 Professional UI • 🔧 Complete Integration • 🚀 Future-Ready**

---

*Built with ❤️ for the EXWM and Emacs communities*
