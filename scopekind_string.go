// Code generated by "stringer -type ScopeKind"; DO NOT EDIT

package gc

import "fmt"

const _ScopeKind_name = "UniverseScopePackageScopeFileScopeBlockScope"

var _ScopeKind_index = [...]uint8{0, 13, 25, 34, 44}

func (i ScopeKind) String() string {
	if i < 0 || i >= ScopeKind(len(_ScopeKind_index)-1) {
		return fmt.Sprintf("ScopeKind(%d)", i)
	}
	return _ScopeKind_name[_ScopeKind_index[i]:_ScopeKind_index[i+1]]
}
