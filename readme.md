# VS Code Introduction, especially for R programming

I have been using VS Code since it launched the first version when I was writing C++. It was not very good at handling compiling and linking tasks for C++ at that time, but it attractted me by its fancy code editing features and its potential to be one of the leading code editors.

Nowadays, VS Code is almost the best editor you can find. It supports thousands of "extensions" developped by programmers all over the world, almost every fancy code editing feature you want can be found in the extension library. 

If you are doing web developing, use html/css/javascript a lot, you are lucky because VS Code has everything you need. However, because of the later start, VS Code hasn't had all features support on R, which is widely used in the data science field (*if you use Python for data science, you are lucky as well, VS Code supports Python very well and it even support jupternotebooks. But, I don't want to argue whether R or Python is the best choice for data science here*).

Even though VS Code hasn't supported everything you can do in RStudio, for example, the R Markdown support is not good yet, it is still a fancy R code editor. At least, give it a try, it'll surprise you!

I recommend reading [this blog](https://renkun.me/2019/12/11/writing-r-in-vscode-a-fresh-start/).

First, we need to install some extensions for writing R code in VS Code.

**Required Extensions**:
* vscode-R (wiki page: https://github.com/Ikuyadeu/vscode-R/wiki/Getting-Started)
* vscode-r-lsp
* languageserver

**Optional Extensions**:
* radian console (need to install it in Python 3)
* #region folding for VS Code
* Better Comments
* Bracket Pair Colorizer 2
* Color Pick
* Quick and Simple Text Selection
* Todo Tree
* Project Manager
* vscode-icons
* Settings Sync (This will save your life!!!)

## Writing R code in VS Code

The best way to introduce code editing features is to show you how I do my R work in VS Code. We will cover:
1. Create a new R script file
2. Basic code editing
   * Multiple-cursor editing
   * Find and replace
   * Quick selection
   * Code folding
   * Color picker
   * Multiple panels
3. Tips
   * Snippets
   * Keybindings
   * Quick blocks selection
   * Command palette
   * Project manage
   * Todo Tree
4. Git support
5. Remote SSH
