# VS Code Introduction, especially for R programming

I have been using VS Code since it launched the first version when I was writing C++. It was not very good at handling compiling and linking tasks for C++ at that time, but it attractted me by its fancy code editing features. It's light but fast. Nowadays, it has become one of the leading code editors.

VSCode supports thousands of "extensions" developped by programmers all over the world, almost all fancy code editing features you want can be found in the extension library. 

If you are doing web developing, use html/css/javascript a lot, you are lucky because VS Code has everything you need. However, because of the later start, VS Code hasn't had all features support on R, which is widely used in the data science field (*if you use Python for data science, you are lucky as well, VS Code supports Python very well and it even support jupyter notebooks. But, I don't want to argue whether R or Python is the best choice for data science here*).

Even though VS Code hasn't supported everything you can do in RStudio, for example, the R Markdown support is not good yet, it is still a nice R code editor. The notebook API for R will come soon. At least, give it a try, it'll surprise you!


## Some extensions for writing R code in VS Code.

> I recommend reading [this blog](https://renkun.me/2019/12/11/writing-r-in-vscode-a-fresh-start/).

**Required Extensions**:
* vscode-R (wiki page: https://github.com/Ikuyadeu/vscode-R/wiki/Getting-Started)
* vscode-r-lsp
* languageserver

**Optional Extensions** (support other languages as well):
* radian console (need to install it in Python 3)
* #region folding for VS Code
* Better Comments
* Bracket Pair Colorizer 2
* Color Pick
* Quick and Simple Text Selection
* Back & Forth
* Todo Tree
* Project Manager
* vscode-icons
* Settings Sync (This will save your life!!!)
* Gist
* Remote SSH
* Markdown all in one
* Draw.io Integration
* vscode-pdf

## Writing R code in VS Code

> There are too many skills I can't cover them all, you can check [this website](https://code.visualstudio.com/docs).
> I'll cover those I use everyday.

1. Git support
2. Basic code editing
   * Multi-cursor editing
   * Find and replace (cmd/ctrl + d)
   * Quick selection
   * Code folding
   * Look up documentation
   * Quick jump
   * Color picker
   * Multiple panels
3. Tips
   * Keybindings
   * Command palette
     * ">", "#", and "@"
   * Snippets & gists
   * Terminal
   * Project manage
   * Todo Tree
4. Remote SSH
5. One more thing
6. If you find any cool skills, please teach me!