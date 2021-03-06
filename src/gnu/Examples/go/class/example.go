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

type SwigcptrShape uintptr

func (p SwigcptrShape) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrShape) SwigIsShape() {
}

func _swig_wrap_delete_Shape(uintptr)

func DeleteShape(arg1 Shape) {
	_swig_wrap_delete_Shape(arg1.Swigcptr())
}

func _swig_wrap_Shape_x_set(SwigcptrShape, float64)

func (arg1 SwigcptrShape) SetX(arg2 float64) {
	_swig_wrap_Shape_x_set(arg1, arg2)
}

func _swig_wrap_Shape_x_get(SwigcptrShape) float64

func (arg1 SwigcptrShape) GetX() float64 {
	return _swig_wrap_Shape_x_get(arg1)
}

func _swig_wrap_Shape_y_set(SwigcptrShape, float64)

func (arg1 SwigcptrShape) SetY(arg2 float64) {
	_swig_wrap_Shape_y_set(arg1, arg2)
}

func _swig_wrap_Shape_y_get(SwigcptrShape) float64

func (arg1 SwigcptrShape) GetY() float64 {
	return _swig_wrap_Shape_y_get(arg1)
}

func _swig_wrap_Shape_move(SwigcptrShape, float64, float64)

func (arg1 SwigcptrShape) Move(arg2 float64, arg3 float64) {
	_swig_wrap_Shape_move(arg1, arg2, arg3)
}

func _swig_wrap_Shape_area(SwigcptrShape) float64

func (arg1 SwigcptrShape) Area() float64 {
	return _swig_wrap_Shape_area(arg1)
}

func _swig_wrap_Shape_perimeter(SwigcptrShape) float64

func (arg1 SwigcptrShape) Perimeter() float64 {
	return _swig_wrap_Shape_perimeter(arg1)
}

func _swig_wrap_Shape_nshapes_set(int)

func SetShapeNshapes(arg1 int) {
	_swig_wrap_Shape_nshapes_set(arg1)
}

func GetShapeNshapes() int
type Shape interface {
	Swigcptr() uintptr
	SwigIsShape()
	SetX(arg2 float64)
	GetX() float64
	SetY(arg2 float64)
	GetY() float64
	Move(arg2 float64, arg3 float64)
	Area() float64
	Perimeter() float64
}

type SwigcptrCircle uintptr

func (p SwigcptrCircle) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrCircle) SwigIsCircle() {
}

func _swig_wrap_new_Circle(float64) SwigcptrCircle

func NewCircle(arg1 float64) Circle {
	return _swig_wrap_new_Circle(arg1)
}

func _swig_wrap_Circle_area(SwigcptrCircle) float64

func (arg1 SwigcptrCircle) Area() float64 {
	return _swig_wrap_Circle_area(arg1)
}

func _swig_wrap_Circle_perimeter(SwigcptrCircle) float64

func (arg1 SwigcptrCircle) Perimeter() float64 {
	return _swig_wrap_Circle_perimeter(arg1)
}

func _swig_wrap_delete_Circle(uintptr)

func DeleteCircle(arg1 Circle) {
	_swig_wrap_delete_Circle(arg1.Swigcptr())
}

func _swig_wrap_SetCircle_X(SwigcptrCircle, float64)

func (_swig_base SwigcptrCircle) SetX(arg1 float64) {
	_swig_wrap_SetCircle_X(_swig_base, arg1)
}

func _swig_wrap_GetCircle_X(SwigcptrCircle) float64

func (_swig_base SwigcptrCircle) GetX() float64 {
	return _swig_wrap_GetCircle_X(_swig_base)
}

func _swig_wrap_SetCircle_Y(SwigcptrCircle, float64)

func (_swig_base SwigcptrCircle) SetY(arg1 float64) {
	_swig_wrap_SetCircle_Y(_swig_base, arg1)
}

func _swig_wrap_GetCircle_Y(SwigcptrCircle) float64

func (_swig_base SwigcptrCircle) GetY() float64 {
	return _swig_wrap_GetCircle_Y(_swig_base)
}

func _swig_wrap_Circle_move(SwigcptrCircle, float64, float64)

func (_swig_base SwigcptrCircle) Move(arg1 float64, arg2 float64) {
	_swig_wrap_Circle_move(_swig_base, arg1, arg2)
}

func (p SwigcptrCircle) SwigIsShape() {
}

func (p SwigcptrCircle) SwigGetShape() Shape {
	return SwigcptrShape(p.Swigcptr())
}

type Circle interface {
	Swigcptr() uintptr
	SwigIsCircle()
	Area() float64
	Perimeter() float64
	SetX(arg1 float64)
	GetX() float64
	SetY(arg1 float64)
	GetY() float64
	Move(arg1 float64, arg2 float64)
	SwigIsShape()
	SwigGetShape() Shape
}

type SwigcptrSquare uintptr

func (p SwigcptrSquare) Swigcptr() uintptr {
	return (uintptr)(p)
}

func (p SwigcptrSquare) SwigIsSquare() {
}

func _swig_wrap_new_Square(float64) SwigcptrSquare

func NewSquare(arg1 float64) Square {
	return _swig_wrap_new_Square(arg1)
}

func _swig_wrap_Square_area(SwigcptrSquare) float64

func (arg1 SwigcptrSquare) Area() float64 {
	return _swig_wrap_Square_area(arg1)
}

func _swig_wrap_Square_perimeter(SwigcptrSquare) float64

func (arg1 SwigcptrSquare) Perimeter() float64 {
	return _swig_wrap_Square_perimeter(arg1)
}

func _swig_wrap_delete_Square(uintptr)

func DeleteSquare(arg1 Square) {
	_swig_wrap_delete_Square(arg1.Swigcptr())
}

func _swig_wrap_SetSquare_X(SwigcptrSquare, float64)

func (_swig_base SwigcptrSquare) SetX(arg1 float64) {
	_swig_wrap_SetSquare_X(_swig_base, arg1)
}

func _swig_wrap_GetSquare_X(SwigcptrSquare) float64

func (_swig_base SwigcptrSquare) GetX() float64 {
	return _swig_wrap_GetSquare_X(_swig_base)
}

func _swig_wrap_SetSquare_Y(SwigcptrSquare, float64)

func (_swig_base SwigcptrSquare) SetY(arg1 float64) {
	_swig_wrap_SetSquare_Y(_swig_base, arg1)
}

func _swig_wrap_GetSquare_Y(SwigcptrSquare) float64

func (_swig_base SwigcptrSquare) GetY() float64 {
	return _swig_wrap_GetSquare_Y(_swig_base)
}

func _swig_wrap_Square_move(SwigcptrSquare, float64, float64)

func (_swig_base SwigcptrSquare) Move(arg1 float64, arg2 float64) {
	_swig_wrap_Square_move(_swig_base, arg1, arg2)
}

func (p SwigcptrSquare) SwigIsShape() {
}

func (p SwigcptrSquare) SwigGetShape() Shape {
	return SwigcptrShape(p.Swigcptr())
}

type Square interface {
	Swigcptr() uintptr
	SwigIsSquare()
	Area() float64
	Perimeter() float64
	SetX(arg1 float64)
	GetX() float64
	SetY(arg1 float64)
	GetY() float64
	Move(arg1 float64, arg2 float64)
	SwigIsShape()
	SwigGetShape() Shape
}


type SwigcptrVoid uintptr
type Void interface {
	Swigcptr() uintptr;
}
func (p SwigcptrVoid) Swigcptr() uintptr {
	return uintptr(p)
}

