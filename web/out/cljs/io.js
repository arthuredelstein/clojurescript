goog.provide('cljs.io');
goog.require('cljs.core');
cljs.io.is_node_QMARK_ = (cljs.core.truth_((function (){try{return require;
}catch (e4679){if(cljs.core.instance_QMARK_.call(null,Error,e4679))
{var e = e4679;
return null;
} else
{if("\uFDD0'else")
{throw e4679;
} else
{return null;
}
}
}})())?true:false);
cljs.io.node_fs = (cljs.core.truth_(cljs.io.is_node_QMARK_)?require("fs"):null);
cljs.io.node_path = (cljs.core.truth_(cljs.io.is_node_QMARK_)?require("path"):null);
cljs.io.path_separator = (cljs.core.truth_(cljs.io.is_node_QMARK_)?cljs.io.node_path.sep:"/");
cljs.io.IFile = {};
cljs.io.file_read = (function file_read(f){
var path = (((function (){var G__4682 = f;
if(G__4682)
{if(cljs.core.truth_((function (){var or__3824__auto__ = null;
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return G__4682.cljs$io$IFile$;
}
})()))
{return true;
} else
{if((!G__4682.cljs$lang$protocol_mask$partition$))
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4682);
} else
{return false;
}
}
} else
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4682);
}
})())?f.getPath():f);
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.readFileSync(path).toString();
} else
{var req = (function (){var G__4683 = (new XMLHttpRequest());
G__4683.open("GET",path,false);
return G__4683;
})();
req.send();
if(cljs.core._EQ_.call(null,200,req.status))
{return req.responseText;
} else
{throw (new Error([cljs.core.str("Could not file-read: "),cljs.core.str(path)].join('')));
}
}
});
cljs.io.file_write = (function file_write(f,data){
var path = (((function (){var G__4685 = f;
if(G__4685)
{if(cljs.core.truth_((function (){var or__3824__auto__ = null;
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return G__4685.cljs$io$IFile$;
}
})()))
{return true;
} else
{if((!G__4685.cljs$lang$protocol_mask$partition$))
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4685);
} else
{return false;
}
}
} else
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4685);
}
})())?f.getPath():f);
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.writeFileSync(path,data);
} else
{throw (new Error("No file-write capability defined for this JS environment"));
}
});
cljs.io.file_exists_QMARK_ = (function file_exists_QMARK_(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.existsSync(path);
} else
{throw (new Error("No file-exists? capability defined for this JS environment"));
}
});
cljs.io.file_is_dir_QMARK_ = (function file_is_dir_QMARK_(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.statSync(path).isDirectory();
} else
{throw (new Error("No file-is-dir? capability defined for this JS environment"));
}
});
cljs.io.file_readdir = (function file_readdir(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.readdirSync(path);
} else
{throw (new Error("No file-readdir capability defined for this JS environment"));
}
});
cljs.io.file_stat = (function file_stat(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.statSync(path);
} else
{throw (new Error("No file-stat capability defined for this JS environment"));
}
});
cljs.io.path_normalize = (function path_normalize(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_path.normalize(path);
} else
{throw (new Error("No path-normalize capability defined for this JS environment"));
}
});
cljs.io.path_resolve = (function path_resolve(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_path.resolve(path);
} else
{throw (new Error("No path-resolve capability defined for this JS environment"));
}
});
cljs.io.path_dirname = (function path_dirname(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_path.dirname(path);
} else
{throw (new Error("No path-dirname capability defined for this JS environment"));
}
});
cljs.io.path_basename = (function path_basename(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_path.basename(path);
} else
{throw (new Error("No path-basename capability defined for this JS environment"));
}
});
cljs.io.mkdir = (function mkdir(path){
if(cljs.core.truth_(cljs.io.is_node_QMARK_))
{return cljs.io.node_fs.mkdirSync(path);
} else
{throw (new Error("No mkdir capability defined for this JS environment"));
}
});
cljs.io.mkdirs = (function mkdirs(path){
var components = path.split("/");
var cur = [cljs.core.str(cljs.core.first.call(null,components)),cljs.core.str("/")].join('');
var left = cljs.core.next.call(null,components);
while(true){
if(cljs.core.not.call(null,cljs.io.file_exists_QMARK_.call(null,cur)))
{cljs.io.mkdir.call(null,cur);
} else
{}
if(left)
{{
var G__4686 = [cljs.core.str(cur),cljs.core.str(cljs.core.first.call(null,left)),cljs.core.str("/")].join('');
var G__4687 = cljs.core.next.call(null,left);
cur = G__4686;
left = G__4687;
continue;
}
} else
{return null;
}
break;
}
});
goog.provide('cljs.io.File');

/**
* @constructor
*/
cljs.io.File = (function (pathname){
this.pathname = pathname;
this.cljs$lang$protocol_mask$partition1$ = 0;
this.cljs$lang$protocol_mask$partition0$ = 2147483648;
})
cljs.io.File.cljs$lang$type = true;
cljs.io.File.cljs$lang$ctorPrSeq = (function (this__1946__auto__){
return cljs.core.list.call(null,"cljs.io/File");
});
cljs.io.File.cljs$lang$ctorPrWriter = (function (this__1946__auto__,writer__1947__auto__,opt__1948__auto__){
return cljs.core._write.call(null,writer__1947__auto__,"cljs.io/File");
});
cljs.io.File.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = (function (me,writer,opts){
var self__ = this;
return cljs.core._write.call(null,writer,[cljs.core.str("#<File "),cljs.core.str(me.getPath()),cljs.core.str(">")].join(''));
});
cljs.io.File.prototype.getParent = (function (){
var self__ = this;
var me = this;
return cljs.io.path_dirname.call(null,me.getPath());
});
cljs.io.File.prototype.getPath = (function (){
var self__ = this;
var _ = this;
return cljs.io.path_normalize.call(null,self__.pathname);
});
cljs.io.File.prototype.mkdirs = (function (){
var self__ = this;
var _ = this;
return cljs.io.mkdirs.call(null,self__.pathname);
});
cljs.io.File.prototype.lastModified = (function (){
var self__ = this;
var _ = this;
return cljs.io.file_stat.call(null,self__.pathname).mtime.getTime();
});
cljs.io.File.prototype.exists = (function (){
var self__ = this;
var me = this;
return cljs.io.file_exists_QMARK_.call(null,me.getPath());
});
cljs.io.File.prototype.listFiles = (function (){
var self__ = this;
var me = this;
return cljs.io.file_readdir.call(null,me.getPath());
});
cljs.io.File.prototype.isDirectory = (function (){
var self__ = this;
var me = this;
return cljs.io.file_is_dir_QMARK_.call(null,me.getPath());
});
cljs.io.File.prototype.getAbsoluteFile = (function (){
var self__ = this;
var me = this;
return (new cljs.io.File(me.getAbsolutePath()));
});
cljs.io.File.prototype.getParentFile = (function (){
var self__ = this;
var me = this;
return (new cljs.io.File(me.getParent()));
});
cljs.io.File.prototype.getCanonicalFile = (function (){
var self__ = this;
var me = this;
return me.getAbsoluteFile();
});
cljs.io.File.prototype.getAbsolutePath = (function (){
var self__ = this;
var me = this;
return cljs.io.path_resolve.call(null,me.getPath());
});
cljs.io.File.prototype.getName = (function (){
var self__ = this;
var me = this;
return cljs.io.path_basename.call(null,me.getPath());
});
cljs.io.File.prototype.toString = (function (){
var self__ = this;
var me = this;
return me.getPath();
});
cljs.io.File.prototype.getCanonicalPath = (function (){
var self__ = this;
var me = this;
return me.getAbsolutePath();
});
cljs.io.File.prototype.cljs$io$IFile$ = true;
cljs.io.file = (function() {
var file = null;
var file__1 = (function (path){
var path__$1 = (((function (){var G__4692 = path;
if(G__4692)
{if(cljs.core.truth_((function (){var or__3824__auto__ = null;
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return G__4692.cljs$io$IFile$;
}
})()))
{return true;
} else
{if((!G__4692.cljs$lang$protocol_mask$partition$))
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4692);
} else
{return false;
}
}
} else
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4692);
}
})())?path.getPath():path);
return (new cljs.io.File(path__$1));
});
var file__2 = (function (parent,path){
var parent__$1 = (((function (){var G__4693 = parent;
if(G__4693)
{if(cljs.core.truth_((function (){var or__3824__auto__ = null;
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return G__4693.cljs$io$IFile$;
}
})()))
{return true;
} else
{if((!G__4693.cljs$lang$protocol_mask$partition$))
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4693);
} else
{return false;
}
}
} else
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4693);
}
})())?parent.getPath():parent);
var path__$1 = (((function (){var G__4694 = path;
if(G__4694)
{if(cljs.core.truth_((function (){var or__3824__auto__ = null;
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return G__4694.cljs$io$IFile$;
}
})()))
{return true;
} else
{if((!G__4694.cljs$lang$protocol_mask$partition$))
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4694);
} else
{return false;
}
}
} else
{return cljs.core.type_satisfies_.call(null,cljs.io.IFile,G__4694);
}
})())?path.getPath():path);
return (new cljs.io.File([cljs.core.str(parent__$1),cljs.core.str(cljs.io.path_separator),cljs.core.str(path__$1)].join('')));
});
file = function(parent,path){
switch(arguments.length){
case 1:
return file__1.call(this,parent);
case 2:
return file__2.call(this,parent,path);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
file.cljs$lang$arity$1 = file__1;
file.cljs$lang$arity$2 = file__2;
return file;
})()
;
/**
* A tree seq on cljs.io.Files
*/
cljs.io.file_seq = (function file_seq(dir){
return cljs.core.tree_seq.call(null,(function (f){
return f.isDirectory();
}),(function (d){
return cljs.core.map.call(null,(function (p1__4688_SHARP_){
return (new cljs.io.File([cljs.core.str(d),cljs.core.str(cljs.io.path_separator),cljs.core.str(p1__4688_SHARP_)].join('')));
}),d.listFiles());
}),dir);
});
cljs.io.resource = (function resource(path){
var p0 = path;
var p1 = [cljs.core.str("out2/"),cljs.core.str(path)].join('');
var p2 = [cljs.core.str("../src/cljs/"),cljs.core.str(path)].join('');
var p3 = [cljs.core.str("../closure/library/closure/"),cljs.core.str(path)].join('');
if(cljs.core.truth_(cljs.io.file_exists_QMARK_.call(null,p0)))
{return p0;
} else
{if(cljs.core.truth_(cljs.io.file_exists_QMARK_.call(null,p1)))
{return p1;
} else
{if(cljs.core.truth_(cljs.io.file_exists_QMARK_.call(null,p2)))
{return p2;
} else
{if(cljs.core.truth_(cljs.io.file_exists_QMARK_.call(null,p3)))
{return p3;
} else
{if("\uFDD0'else")
{return null;
} else
{return null;
}
}
}
}
}
});

// Analyzer namespace snapshot:
cljs.core.swap_BANG_.call(null,cljs.core.namespaces,cljs.core.update_in,cljs.core.PersistentVector.fromArray([(new cljs.core.Symbol(null,"cljs.io"))], true),(function (old){
return cljs.core.deep_merge_with.call(null,(function() { 
var G__4695__delegate = function (m){
return cljs.core.first.call(null,m);
};
var G__4695 = function (var_args){
var m = null;
if (goog.isDef(var_args)) {
  m = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0),0);
} 
return G__4695__delegate.call(this, m);
};
G__4695.cljs$lang$maxFixedArity = 0;
G__4695.cljs$lang$applyTo = (function (arglist__4696){
var m = cljs.core.seq(arglist__4696);;
return G__4695__delegate(m);
});
G__4695.cljs$lang$arity$variadic = G__4695__delegate;
return G__4695;
})()
,cljs.core.hash_map("\uFDD0'defs",cljs.core.hash_map((new cljs.core.Symbol(null,"file-write")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"f")),(new cljs.core.Symbol(null,"data"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"f")),"\uFDD0'tag",null,"\uFDD0'shadow",null),cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"data")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-write")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",2,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",22,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]),cljs.core.vec([(new cljs.core.Symbol(null,"parent")),(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)]),cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"parent")),"\uFDD0'tag",null,"\uFDD0'shadow",null),cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",2,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",164,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"path-dirname")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/path-dirname")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",65,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"is-node?")),cljs.core.hash_map("\uFDD0'line",3,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/is-node?"))),(new cljs.core.Symbol(null,"mkdirs")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/mkdirs")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",83,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"/")),cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"cljs.core//"))),(new cljs.core.Symbol(null,"mkdir")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/mkdir")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",77,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"node-path")),cljs.core.hash_map("\uFDD0'line",5,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/node-path"))),(new cljs.core.Symbol(null,"file-read")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"f"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"f")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-read")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",10,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file-is-dir?")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-is-dir?")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",35,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"path-resolve")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/path-resolve")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",59,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file-exists?")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-exists?")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",29,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"node-fs")),cljs.core.hash_map("\uFDD0'line",4,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/node-fs"))),(new cljs.core.Symbol(null,"resource")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/resource")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",185,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file-seq")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"dir"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"dir")),"\uFDD0'tag",(new cljs.core.Symbol(null,"cljs.io.File")),"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-seq")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'doc","A tree seq on cljs.io.Files","\uFDD0'line",173,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file-readdir")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-readdir")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",41,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"path-separator")),cljs.core.hash_map("\uFDD0'line",7,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/path-separator"))),(new cljs.core.Symbol(null,"path-normalize")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/path-normalize")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",53,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"IFile")),cljs.core.hash_map("\uFDD0'line",8,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'protocol-symbol",true,"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/IFile"))),(new cljs.core.Symbol(null,"File")),cljs.core.hash_map("\uFDD0'line",93,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs","\uFDD0'protocols",cljs.core.set([(new cljs.core.Symbol(null,"cljs.io/Object")),(new cljs.core.Symbol(null,"cljs.io/IFile")),(new cljs.core.Symbol(null,"cljs.core/IPrintWithWriter"))]),"\uFDD0'num-fields",1,"\uFDD0'type",true,"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/File"))),(new cljs.core.Symbol(null,"path-basename")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/path-basename")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",71,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs"),(new cljs.core.Symbol(null,"file-stat")),cljs.core.hash_map("\uFDD0'arglists",cljs.core.list((new cljs.core.Symbol(null,"quote")),cljs.core.list(cljs.core.vec([(new cljs.core.Symbol(null,"path"))]))),"\uFDD0'method-params",cljs.core.list(cljs.core.vec([cljs.core.hash_map("\uFDD0'name",(new cljs.core.Symbol(null,"path")),"\uFDD0'tag",null,"\uFDD0'shadow",null)])),"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io/file-stat")),"\uFDD0'protocol-impl",null,"\uFDD0'max-fixed-arity",1,"\uFDD0'private",true,"\uFDD0'protocol-inline",null,"\uFDD0'variadic",false,"\uFDD0'line",47,"\uFDD0'fn-var",true,"\uFDD0'file","/projects/cljs6.net/clojurescript/src/cljs/cljs/io.cljs")),"\uFDD0'imports",null,"\uFDD0'uses-macros",null,"\uFDD0'requires",null,"\uFDD0'uses",null,"\uFDD0'excludes",cljs.core.set([]),"\uFDD0'doc",null,"\uFDD0'name",(new cljs.core.Symbol(null,"cljs.io"))),old);
}));
