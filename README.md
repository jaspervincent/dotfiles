使用 [GNU Stow](https://www.gnu.org/software/stow/) 管理配置文件.

## Emacs设置

在`~/.emacs.d` 目录中创建指向我的配置文件的符号链接

```bash
stow -t "$HOME" emacs
```

文件内容更新， 重新创建符号链接。 -R 相当于执行了 -D 删除再执行创建命令

```bash
stow -R -t "$HOME" emacs
```

注意：PC中创建的是非符号链接。执行反向同步追加，使用`--adopt`选项

```bash
stow -R  --adopt -t $HOME emacs/
```


## 许可证

除非另有说明，否则此处的所有代码均根据 GNU 通用公共许可证版本 3 或更高版本的条款分发。
