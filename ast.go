// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

var (
	_ Declaration = (*ConstDecl)(nil)
	_ Declaration = (*FuncDecl)(nil)
	_ Declaration = (*ImportDecl)(nil)
	_ Declaration = (*MethodDecl)(nil)
	_ Declaration = (*TypeDecl)(nil)
	_ Declaration = (*VarDecl)(nil)
)

type declaration struct {
	off int
	src *SourceFile
}

func newDeclaration(src *SourceFile, off int) declaration {
	return declaration{
		off,
		src,
	}
}

// Const implements Declaration.
func (d *declaration) Const() *ConstDecl { panic("Const of inappropriate Declaration") }

// Func implements Declaration.
func (d *declaration) Func() *FuncDecl { panic("Func of inappropriate Declaration") }

// Method implements Declaration.
func (d *declaration) Method() *MethodDecl { panic("Method of inappropriate Declaration") }

// Import implements Declaration.
func (d *declaration) Import() *ImportDecl { panic("Import of inappropriate Declaration") }

// Offset implements Declaration.
func (d *declaration) Offset() int { return d.off }

// SourceFile implements Declaration.
func (d *declaration) SourceFile() *SourceFile { return d.src }

// Type implements Declaration.
func (d *declaration) Type() *TypeDecl { panic("Type of inappropriate Declaration") }

// Var implements Declaration.
func (d *declaration) Var() *VarDecl { panic("Var of inappropriate Declaration") }

// ImportDecl describes an import declaration.
type ImportDecl struct {
	ImportPath  string
	PackageName string // `foo "bar/baz"`
	Dot         bool   // `. "foo/bar"`

	declaration
}

func newImportDecl(src *SourceFile, off int, dot bool, pkgName, importPath string) *ImportDecl {
	return &ImportDecl{
		Dot:         dot,
		ImportPath:  importPath,
		PackageName: pkgName,
		declaration: newDeclaration(src, off),
	}
}

// Import implements Declaration.
func (d *ImportDecl) Import() *ImportDecl { return d }

// Kind implements Declaration.
func (d *ImportDecl) Kind() DeclarationKind { return ImportDeclaration }

// ConstDecl describes a constant declaration.
type ConstDecl struct {
	declaration
}

func newConstDecl(src *SourceFile, off int) *ConstDecl {
	return &ConstDecl{
		declaration: newDeclaration(src, off),
	}
}

// Const implements Declaration.
func (d *ConstDecl) Const() *ConstDecl { return d }

// Kind implements Declaration.
func (d *ConstDecl) Kind() DeclarationKind { return ConstDeclaration }

// FuncDecl describes a function declaration.
type FuncDecl struct {
	declaration
}

func newFuncDecl(src *SourceFile, off int) *FuncDecl {
	return &FuncDecl{
		declaration: newDeclaration(src, off),
	}
}

// Func implements Declaration.
func (d *FuncDecl) Func() *FuncDecl { return d }

// Kind implements Declaration.
func (d *FuncDecl) Kind() DeclarationKind { return FuncDeclaration }

// MethodDecl describes a method declaration.
type MethodDecl struct {
	declaration
}

func newMethodDecl(src *SourceFile, off int) *MethodDecl {
	return &MethodDecl{
		declaration: newDeclaration(src, off),
	}
}

// Kind implements Declaration.
func (d *MethodDecl) Kind() DeclarationKind { return MethodDeclaration }

// Method implements Declaration.
func (d *MethodDecl) Method() *MethodDecl { return d }

// TypeDecl describes a type declaration.
type TypeDecl struct {
	declaration
}

func newTypeDecl(src *SourceFile, off int) *TypeDecl {
	return &TypeDecl{
		declaration: newDeclaration(src, off),
	}
}

// Kind implements Declaration.
func (d *TypeDecl) Kind() DeclarationKind { return TypeDeclaration }

// Type implements Declaration.
func (d *TypeDecl) Type() *TypeDecl { return d }

// VarDecl describes a variable declaration.
type VarDecl struct {
	declaration
}

func newVarDecl(src *SourceFile, off int) *VarDecl {
	return &VarDecl{
		declaration: newDeclaration(src, off),
	}
}

// Kind implements Declaration.
func (d *VarDecl) Kind() DeclarationKind { return VarDeclaration }

// Var implements Declaration.
func (d *VarDecl) Var() *VarDecl { return d }

// Declaration describes a constant, function, method, type or variable
// declaration.
type Declaration interface {
	// Const returns the Declaration's *ConstDecl. It panics if Kind is not
	// ConstDeclaration.
	Const() *ConstDecl

	// Func returns the Declaration's  *FuncDecl. It panics if Kind is not
	// FuncDeclaration.
	Func() *FuncDecl

	// Import returns the Declaration's  *ImportDecl. It panics if Kind is
	// not ImportDeclaration.
	Import() *ImportDecl

	// Kind returns the Declarations's kind.
	Kind() DeclarationKind

	// Method returns the Declaration's  *MethodDecl. It panics if Kind is
	// not MethodDeclaration.
	Method() *MethodDecl

	// Offset returns the Delcaration's offset within its SourceFile.
	Offset() int // Position in source file.

	// SourceFile returns the Declaration's SourceFile.
	SourceFile() *SourceFile

	// Type returns the Declaration's  *TypeDecl. It panics if Kind is not
	// TypeDeclaration.
	Type() *TypeDecl

	// Var returns the Declaration's  *VarDecl. It panics if Kind is not
	// VarDeclaration.
	Var() *VarDecl
}

// SourceFile describes a source file.
type SourceFile struct {
	Package       *Package
	ImportDecls   []*ImportDecl
	TopLevelDecls []Declaration
}

func newSourceFile(pkg *Package) *SourceFile {
	return &SourceFile{
		Package: pkg,
	}
}

func (s *SourceFile) init(pkg *Package) {
	s.Package = pkg
	s.ImportDecls = s.ImportDecls[:0]
	s.TopLevelDecls = s.TopLevelDecls[:0]
}

// Package describes a package.
type Package struct {
	Name        string
	SourceFiles []*SourceFile
}

func newPackage(name string) *Package {
	return &Package{
		Name: name,
	}
}
