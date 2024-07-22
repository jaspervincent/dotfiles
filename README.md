使用 [GNU Stow](https://www.gnu.org/software/stow/) 管理配置文件.

## Emacs设置

在`~/.emacs.d` 目录中创建指向我的配置文件的符号链接

```bash
stow -t "$HOME" emacs
```

更新我的emacs

```bash
stow -t "$HOME" -R emacs
```

## 许可证

除非另有说明，否则此处的所有代码均根据 GNU 通用公共许可证版本 3 或更高版本的条款分发。
