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

func _swig_wrap_ivar_set(int)

func SetIvar(arg1 int) {
	_swig_wrap_ivar_set(arg1)
}

func GetIvar() int
func _swig_wrap_svar_set(int16)

func SetSvar(arg1 int16) {
	_swig_wrap_svar_set(arg1)
}

func GetSvar() int16
func _swig_wrap_lvar_set(int32)

func SetLvar(arg1 int32) {
	_swig_wrap_lvar_set(arg1)
}

func GetLvar() int32
func _swig_wrap_uivar_set(uint)

func SetUivar(arg1 uint) {
	_swig_wrap_uivar_set(arg1)
}

func GetUivar() uint
func _swig_wrap_usvar_set(uint16)

func SetUsvar(arg1 uint16) {
	_swig_wrap_usvar_set(arg1)
}

func GetUsvar() uint16
func _swig_wrap_ulvar_set(uint32)

func SetUlvar(arg1 uint32) {
	_swig_wrap_ulvar_set(arg1)
}

func GetUlvar() uint32
func _swig_wrap_scvar_set(int8)

func SetScvar(arg1 int8) {
	_swig_wrap_scvar_set(arg1)
}

func GetScvar() int8
func _swig_wrap_ucvar_set(byte)

func SetUcvar(arg1 byte) {
	_swig_wrap_ucvar_set(arg1)
}

func GetUcvar() byte
func _swig_wrap_cvar_set(byte)

func SetCvar(arg1 byte) {
	_swig_wrap_cvar_set(arg1)
}

func GetCvar() byte
func _swig_wrap_fvar_set(float32)

func SetFvar(arg1 float32) {
	_swig_wrap_fvar_set(arg1)
}

func GetFvar() float32
func _swig_wrap_dvar_set(float64)

func SetDvar(arg1 float64) {
	_swig_wrap_dvar_set(arg1)
}

func GetDvar() float64
func _swig_wrap_strvar_set(string)

func SetStrvar(arg1 string) {
	_swig_wrap_strvar_set(arg1)
}

func GetStrvar() string
func GetCstrvar() string
func _swig_wrap_iptrvar_set(*int)

func SetIptrvar(arg1 *int) {
	_swig_wrap_iptrvar_set(arg1)
}

func GetIptrvar() *int
func _swig_wrap_name_set(string)

func SetName(arg1 string) {
	_swig_wrap_name_set(arg1)
}

func GetName() string
func _swig_wrap_ptptr_set(uintptr)

func SetPtptr(arg1 Point) {
	_swig_wrap_ptptr_set(arg1.Swigcptr())
}

func _swig_wrap_ptptr_get() SwigcptrPoint

func GetPtptr() Point {
	return _swig_wrap_ptptr_get()
}

func _swig_wrap_pt_set(uintptr)

func SetPt(arg1 Point) {
	_swig_wrap_pt_set(arg1.Swigcptr())
}

func _swig_wrap_pt_get() SwigcptrPoint

func GetPt() Point {
	return _swig_wrap_pt_get()
}

func GetStatus() int
func GetPath() string
func _swig_wrap_print_vars()

func Print_vars() {
	_swig_wrap_print_vars()
}

func New_int(int) *int
func _swig_wrap_new_Point(int, int) SwigcptrPoint

func New_Point(arg1 int, arg2 int) Point {
	return _swig_wrap_new_Point(arg1, arg2)
}

func _swig_wrap_Point_print(uintptr) string

func Point_print(arg1 Point) string {
	return _swig_wrap_Point_print(arg1.Swigcptr())
}

func _swig_wrap_pt_print()

func Pt_print() {
	_swig_wrap_pt_print()
}


type SwigcptrPoint uintptr
type Point interface {
	Swigcptr() uintptr;
}
func (p SwigcptrPoint) Swigcptr() uintptr {
	return uintptr(p)
}

type SwigcptrVoid uintptr
type Void interface {
	Swigcptr() uintptr;
}
func (p SwigcptrVoid) Swigcptr() uintptr {
	return uintptr(p)
}

