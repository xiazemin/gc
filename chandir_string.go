// Code generated by "stringer -type ChanDir"; DO NOT EDIT

package gc

import "fmt"

const _ChanDir_name = "RecvDirSendDir"

var _ChanDir_index = [...]uint8{0, 7, 14}

func (i ChanDir) String() string {
	i -= 1
	if i < 0 || i >= ChanDir(len(_ChanDir_index)-1) {
		return fmt.Sprintf("ChanDir(%d)", i+1)
	}
	return _ChanDir_name[_ChanDir_index[i]:_ChanDir_index[i+1]]
}
