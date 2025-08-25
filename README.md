# 🚀 Emacs AI 2.0 + EXWM - Complete Desktop Environment

[![Emacs Version](https://img.shields.io/badge/Emacs-27.1+-blue.svg)](https://www.gnu.org/software/emacs/)
[![EXWM](https://img.shields.io/badge/Window%20Manager-EXWM-green.svg)](https://github.com/ch11ng/exwm)
[![Elpaca](https://img.shields.io/badge/Package%20Manager-Elpaca%20%2B%20Fallback-green.svg)](https://github.com/progfolio/elpaca)
[![License](https://img.shields.io/badge/License-GPL%203.0-green.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-brightgreen.svg)](https://github.com/krispi/emacs-config)
[![Desktop](https://img.shields.io/badge/Desktop%20Environment-Complete-orange.svg)](#exwm-desktop-environment)

> **🌟 Complete Emacs-based desktop environment featuring the most advanced AI-powered Emacs configuration with EXWM window manager, providing a seamless and productive computing experience.**

## ✨ What You Get

This project provides a **complete desktop environment** built around Emacs and EXWM, combining:

- 🎨 **Premium Emacs Configuration** - Most advanced AI-powered setup available
- 🖥️ **EXWM Window Manager** - Tiling window management integrated with Emacs
- 🚀 **Complete Desktop Environment** - From login to productivity
- 🤖 **AI-First Development** - Modern AI tools and next-generation language support
- 🎪 **Professional Visual Experience** - Premium UI with stunning effects

## 🏗️ Project Structure

```
emacsAI_2/
├── 📁 emacs_config/           # Complete Emacs configuration
│   ├── 📄 init.el             # Main Emacs entry point
│   ├── 📁 lisp/               # Modular configuration modules
│   ├── 📋 Makefile            # Build and maintenance commands
│   └── 📖 README.md           # Detailed Emacs documentation
├── 📁 Exwm/                   # EXWM desktop environment
│   ├── 📄 exwm-config.el      # Core EXWM configuration
│   ├── 📄 elpaca-integration.el # Package management integration
│   ├── 📄 system-integration.el # System-level integration
│   ├── 📄 exwm-keybindings.el # All keybindings and shortcuts
│   ├── 📁 config/             # System configuration files
│   │   └── 📁 exwm/
│   │       ├── 📄 autostart-improved.sh  # Enhanced autostart script
│   │       └── 📄 exwm-powermenu.sh     # Power management menu
│   ├── 📄 lightdm-entry.txt   # LightDM session entry
│   ├── 📄 xinitrc.sh          # Enhanced X11 startup script
│   ├── 📄 picom-config.txt    # Picom compositor configuration
│   ├── 📄 rofi-powermenu.txt  # Rofi power menu styling
│   └── 📖 README.md           # EXWM-specific documentation
├── 📖 README.md               # This main documentation
└── 📄 .gitignore              # Git ignore patterns
```

## 🚀 Quick Start

### 🔧 System Requirements

- **Linux distribution** (Arch Linux recommended)
- **Emacs 27.1+** (29.x or 30.x for best experience)
- **X11** or **Wayland** (X11 recommended for EXWM)
- **Git** for package management
- **Modern terminal** with Unicode support

### 📦 Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/krispi/emacsAI_2.git ~/.emacs.d
   cd ~/.emacs.d
   ```

2. **Install Emacs and dependencies**
   ```bash
   # Arch Linux
   sudo pacman -S emacs exwm xorg-server xorg-xinit rofi picom dunst
   
   # Ubuntu/Debian
   sudo apt install emacs exwm xorg xinit rofi picom dunst
   ```

3. **Configure LightDM (optional)**
   ```bash
   # Copy desktop entry
   sudo cp Exwm/lightdm-entry.txt /usr/share/xsessions/emacs-exwm.desktop
   
   # Enable LightDM
   sudo systemctl enable lightdm
   ```

4. **Start the environment**
   ```bash
   # Direct start
   ./Exwm/xinitrc.sh
   
   # Or through LightDM
   # Select "Emacs + EXWM" from login screen
   ```

## 🎯 Key Features

### 🎨 **Emacs Configuration**
- **AI Integration**: ChatGPT, LLM, and AI-powered development
- **Premium UI**: Beautiful themes with visual effects
- **Modern Languages**: Support for TOP 2025 languages (Mojo, Carbon, V, etc.)
- **LSP Everywhere**: Full Language Server Protocol support
- **Professional Tools**: DAP debugging, linting, and development

### 🖥️ **EXWM Desktop Environment**
- **Tiling Windows**: Automatic window management
- **Workspace Support**: 4 virtual workspaces
- **Rofi Integration**: Application launcher and window switcher
- **System Tray**: Integrated system status
- **Power Management**: Built-in power menu
- **Auto-start**: Automatic application launching

### 🔑 **Key Bindings**

#### **EXWM Global Keys**
| Key | Function | Description |
|-----|----------|-------------|
| `Super + 1-4` | Workspace Switch | Switch to workspace 1-4 |
| `Super + p` | Rofi Launcher | Launch applications |
| `Super + Tab` | Window Switcher | Switch between windows |
| `Super + x` | Power Menu | System power options |
| `Super + Return` | Terminal | Open Alacritty terminal |
| `Super + q` | Close Window | Close current window |
| `Super + r` | Reload Config | Reload EXWM configuration |
| `Super + l` | Lock Screen | Lock the screen |

#### **Emacs Keys (Space Leader)**
| Key | Function | Description |
|-----|----------|-------------|
| `SPACE f f` | Find File | Quick file navigation |
| `SPACE p p` | Projectile | Project management |
| `SPACE b b` | Switch Buffer | Buffer switching |
| `SPACE s s` | Search | Text search |
| `F8` | File Tree | Toggle Neotree |

## 🎪 EXWM Desktop Environment

### 🌟 **What is EXWM?**

EXWM (Emacs X Window Manager) transforms Emacs into a complete window manager, providing:

- **Seamless Integration**: Windows become Emacs buffers
- **Tiling Management**: Automatic window arrangement
- **Emacs Control**: All window operations through Emacs
- **Workspace Support**: Virtual desktop management
- **Application Integration**: Native X11 applications work seamlessly

### 🏠 **Desktop Features**

#### **Workspace Management**
- **4 Virtual Workspaces**: Organized by task type
- **Automatic Layout**: Smart window arrangement
- **Easy Navigation**: Super + 1-4 to switch workspaces
- **Persistent Layouts**: Remember window arrangements

#### **Application Launcher**
- **Rofi Integration**: Modern application launcher
- **Window Switcher**: Quick window navigation
- **Power Menu**: System shutdown, restart, lock
- **Custom Scripts**: Extensible launcher system

#### **System Integration**
- **System Tray**: Integrated status indicators
- **Auto-start**: Automatic application launching
- **Power Management**: Built-in power controls
- **Screen Management**: Multi-monitor support

### 🔧 **Configuration Files**

#### **Core Configuration**
- **exwm-config.el**: Main EXWM configuration
- **system-integration.el**: System tray, notifications, power management
- **exwm-keybindings.el**: All keybindings and shortcuts
- **elpaca-integration.el**: Package management and dependencies

#### **System Scripts**
- **autostart-improved.sh**: Enhanced application startup script
- **exwm-powermenu.sh**: Robust power management menu
- **xinitrc.sh**: Professional X11 startup script

#### **Visual Configuration**
- **picom-config.txt**: Optimized compositor configuration
- **rofi-powermenu.txt**: Beautiful power menu styling
- **lightdm-entry.txt**: Professional LightDM session entry

#### **System Integration**
- **lightdm-entry.txt**: LightDM session configuration
- **config/exwm/**: System configuration files

## 🚀 Advanced Features

### 🤖 **AI Development Environment**
- **GPTel**: ChatGPT integration
- **LLM.el**: Large Language Model support
- **Org-AI**: AI-powered Org mode
- **Code Completion**: AI-assisted coding

### 🌐 **Next-Generation Languages**
- **Mojo**: AI-first language (35000x Python speed)
- **Carbon**: Google's C++ successor
- **V**: Fast, safe language (Go + C performance)
- **Nim**: Expressive language (Python + C performance)
- **Crystal**: Type-safe language (Ruby + C speed)

### 🛠️ **Development Tools**
- **LSP Everywhere**: Full language support
- **DAP Debugging**: Advanced debugging
- **Code Formatting**: Automatic formatting
- **Linting**: Real-time error checking

## 🔧 Customization

### **Emacs Configuration**
- **Modular Design**: Easy to modify individual components
- **Professional Defaults**: Sensible out-of-the-box settings
- **Extensible**: Simple to add new features
- **Documentation**: Comprehensive module documentation

### **EXWM Configuration**
- **Key Bindings**: Customize global shortcuts
- **Workspace Layouts**: Modify workspace behavior
- **Application Rules**: Control window placement
- **Auto-start**: Configure startup applications

### **System Integration**
- **LightDM**: Customize login experience
- **Rofi**: Personalize application launcher
- **Picom**: Configure visual effects
- **Auto-start**: Manage startup applications

## 📚 Documentation

### **Emacs Configuration**
- [Detailed Emacs README](emacs_config/README.md) - Complete Emacs documentation
- **Module Documentation**: Each module includes comprehensive docs
- **Key Bindings**: Complete keyboard shortcut reference
- **Architecture**: Detailed system architecture

### **EXWM Configuration**
- [EXWM README](Exwm/README.md) - Complete EXWM documentation
- **Configuration Files**: All EXWM setup files documented
- **Integration Guide**: System integration instructions
- **Customization**: How to modify and extend
- **Troubleshooting**: Common issues and solutions

## 🚀 Getting Started

### **1. Basic Setup**
```bash
# Clone the repository
git clone https://github.com/krispi/emacsAI_2.git ~/.emacs.d

# Install dependencies
sudo pacman -S emacs exwm xorg-server xorg-xinit rofi picom dunst

# Start the environment
cd ~/.emacs.d
./Exwm/xinitrc.sh
```

### **2. LightDM Integration**
```bash
# Copy desktop entry
sudo cp Exwm/lightdm-entry.txt /usr/share/xsessions/emacs-exwm.desktop

# Enable LightDM
sudo systemctl enable lightdm

# Reboot and select "Emacs + EXWM"
```

### **3. First Experience**
- **Login**: Select "Emacs + EXWM" from LightDM
- **Workspace**: Use Super + 1-4 to navigate
- **Applications**: Super + p to launch apps
- **Terminal**: Super + Return for terminal
- **Files**: F8 for file tree in Emacs

## 🤝 Contributing

### **Guidelines**
1. **Maintain modularity**: Keep changes isolated
2. **Follow standards**: Use professional coding practices
3. **Document changes**: Update documentation
4. **Test thoroughly**: Ensure no regressions

### **Development**
- **Fork the repository**
- **Create feature branch**
- **Make your changes**
- **Submit pull request**

## 📄 License

This project is licensed under the **GNU General Public License v3.0** - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Emacs Community**: For the amazing ecosystem
- **EXWM Team**: For the excellent window manager
- **Elpaca Team**: For the package manager
- **Package Authors**: For all the wonderful packages
- **Contributors**: For helping improve this configuration

## 📞 Support

### **Issues**
- **GitHub Issues**: Report bugs and request features
- **Documentation**: Check this README first
- **Community**: Join Emacs and EXWM discussions

### **Contact**
- **GitHub**: [krispi/emacsAI_2](https://github.com/krispi/emacsAI_2)
- **Issues**: [GitHub Issues](https://github.com/krispi/emacsAI_2/issues)

---

## 🎉 **Ready for the Ultimate Emacs Experience?**

**Transform your computer into a powerful, AI-powered development environment with the most advanced Emacs configuration and seamless EXWM integration!**

**🚀 Clone • 🎯 Configure • ✨ Enjoy • 🖥️ Transform**

**🎨 Premium UI • 🤖 AI-First • 🖥️ Complete Desktop • 🚀 Future-Ready**

---

*Built with ❤️ for the Emacs and Linux communities*
