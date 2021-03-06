/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.1
 * 
 * This file is not intended to be easily readable and contains a number of 
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG 
 * interface file instead. 
 * ----------------------------------------------------------------------------- */

package example


type _swig_fnptr *byte
type _swig_memberptr *byte


func _swig_allocatememory(int) *byte
func _swig_internal_allocate(len int) *byte {
	return _swig_allocatememory(len)
}

func _swig_allocatestring(*byte, int) string
func _swig_internal_makegostring(p *byte, l int) string {
	return _swig_allocatestring(p, l)
}

func _swig_internal_gopanic(p *byte, l int) {
	panic(_swig_allocatestring(p, l))
}

type Color int
func _swig_getRED() Color
var RED Color = _swig_getRED()
func _swig_getBLUE() Color
var BLUE Color = _swig_getBLUE()
func _swig_getGREEN() Color
var GREEN Color = _swig_getGREEN()
type SwigcptrFoo uintptr

func (p SwigcptrFoo) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrFoo) SwigIsFoo() {
}

func _swig_wrap_new_Foo() SwigcptrFoo

func NewFoo() Foo {
	return _swig_wrap_new_Foo()
}

type FooSpeed int
func _swig_getFoo_IMPULSE_Foo() FooSpeed
var FooIMPULSE FooSpeed = _swig_getFoo_IMPULSE_Foo()
func _swig_getFoo_WARP_Foo() FooSpeed
var FooWARP FooSpeed = _swig_getFoo_WARP_Foo()
func _swig_getFoo_LUDICROUS_Foo() FooSpeed
var FooLUDICROUS FooSpeed = _swig_getFoo_LUDICROUS_Foo()
func _swig_wrap_Foo_enum_test(SwigcptrFoo, FooSpeed)

func (arg1 SwigcptrFoo) Enum_test(arg2 FooSpeed) {
	_swig_wrap_Foo_enum_test(arg1, arg2)
}

func _swig_wrap_delete_Foo(uintptr)

func DeleteFoo(arg1 Foo) {
	_swig_wrap_delete_Foo(arg1.Swigcptr())
}

type Foo interface {
	Swigcptr() uintptr
	SwigIsFoo()
	Enum_test(arg2 FooSpeed)
}

func _swig_wrap_enum_test(Color, FooSpeed)

func Enum_test(arg1 Color, arg2 FooSpeed) {
	_swig_wrap_enum_test(arg1, arg2)
}


type SwigcptrVoid uintptr
type Void interface {
	Swigcptr() uintptr;
}
func (p SwigcptrVoid) Swigcptr() uintptr {
	return uintptr(p)
}

