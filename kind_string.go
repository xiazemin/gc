// Code generated by "stringer -type Kind"; DO NOT EDIT

package gc

import "fmt"

const _Kind_name = "InvalidBoolIntInt8Int16Int32Int64UintUint8Uint16Uint32Uint64UintptrFloat32Float64Complex64Complex128ArrayChanFuncInterfaceMapPtrSliceStringStructUnsafePointerTupleUntypedBoolmaxKind"

var _Kind_index = [...]uint8{0, 7, 11, 14, 18, 23, 28, 33, 37, 42, 48, 54, 60, 67, 74, 81, 90, 100, 105, 109, 113, 122, 125, 128, 133, 139, 145, 158, 163, 174, 181}

func (i Kind) String() string {
	if i >= Kind(len(_Kind_index)-1) {
		return fmt.Sprintf("Kind(%d)", i)
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
