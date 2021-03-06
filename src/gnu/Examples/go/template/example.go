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

func Maxint(int, int) int
func Maxdouble(float64, float64) float64
type SwigcptrVecint uintptr

func (p SwigcptrVecint) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrVecint) SwigIsVecint() {
}

func _swig_wrap_new_vecint(int) SwigcptrVecint

func NewVecint(arg1 int) Vecint {
	return _swig_wrap_new_vecint(arg1)
}

func _swig_wrap_vecint_get(SwigcptrVecint, int) *int

func (arg1 SwigcptrVecint) Get(arg2 int) *int {
	return _swig_wrap_vecint_get(arg1, arg2)
}

func _swig_wrap_vecint_set(SwigcptrVecint, int, *int)

func (arg1 SwigcptrVecint) Set(arg2 int, arg3 *int) {
	_swig_wrap_vecint_set(arg1, arg2, arg3)
}

func _swig_wrap_vecint_getitem(SwigcptrVecint, int) int

func (arg1 SwigcptrVecint) Getitem(arg2 int) int {
	return _swig_wrap_vecint_getitem(arg1, arg2)
}

func _swig_wrap_vecint_setitem(SwigcptrVecint, int, int)

func (arg1 SwigcptrVecint) Setitem(arg2 int, arg3 int) {
	_swig_wrap_vecint_setitem(arg1, arg2, arg3)
}

func _swig_wrap_delete_vecint(uintptr)

func DeleteVecint(arg1 Vecint) {
	_swig_wrap_delete_vecint(arg1.Swigcptr())
}

type Vecint interface {
	Swigcptr() uintptr
	SwigIsVecint()
	Get(arg2 int) *int
	Set(arg2 int, arg3 *int)
	Getitem(arg2 int) int
	Setitem(arg2 int, arg3 int)
}

type SwigcptrVecdouble uintptr

func (p SwigcptrVecdouble) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrVecdouble) SwigIsVecdouble() {
}

func _swig_wrap_new_vecdouble(int) SwigcptrVecdouble

func NewVecdouble(arg1 int) Vecdouble {
	return _swig_wrap_new_vecdouble(arg1)
}

func _swig_wrap_vecdouble_get(SwigcptrVecdouble, int) *float64

func (arg1 SwigcptrVecdouble) Get(arg2 int) *float64 {
	return _swig_wrap_vecdouble_get(arg1, arg2)
}

func _swig_wrap_vecdouble_set(SwigcptrVecdouble, int, *float64)

func (arg1 SwigcptrVecdouble) Set(arg2 int, arg3 *float64) {
	_swig_wrap_vecdouble_set(arg1, arg2, arg3)
}

func _swig_wrap_vecdouble_getitem(SwigcptrVecdouble, int) float64

func (arg1 SwigcptrVecdouble) Getitem(arg2 int) float64 {
	return _swig_wrap_vecdouble_getitem(arg1, arg2)
}

func _swig_wrap_vecdouble_setitem(SwigcptrVecdouble, int, float64)

func (arg1 SwigcptrVecdouble) Setitem(arg2 int, arg3 float64) {
	_swig_wrap_vecdouble_setitem(arg1, arg2, arg3)
}

func _swig_wrap_delete_vecdouble(uintptr)

func DeleteVecdouble(arg1 Vecdouble) {
	_swig_wrap_delete_vecdouble(arg1.Swigcptr())
}

type Vecdouble interface {
	Swigcptr() uintptr
	SwigIsVecdouble()
	Get(arg2 int) *float64
	Set(arg2 int, arg3 *float64)
	Getitem(arg2 int) float64
	Setitem(arg2 int, arg3 float64)
}


type SwigcptrVoid uintptr
type Void interface {
	Swigcptr() uintptr;
}
func (p SwigcptrVoid) Swigcptr() uintptr {
	return uintptr(p)
}

