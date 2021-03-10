package tree

import (
	"errors"
	"sort"
)

// Record is the input element for tree-building
type Record struct {
	ID     int
	Parent int
}

// Node is the output structure for tree-building
type Node struct {
	ID       int
	Children []*Node
}

// Stupid lint gets stupid comments:

// ErrRootNotUnique does what its name says
var ErrRootNotUnique = errors.New("root not unique")

// ErrParentIDHigher does what its name says
var ErrParentIDHigher = errors.New("parent node id is higher")

// ErrIDNotContinuous does what its name says
var ErrIDNotContinuous = errors.New("record ids are not continuous")

// Build consumes input Records and builds a structural representation.
func Build(records []Record) (*Node, error) {
	if len(records) == 0 {
		return nil, nil
	}

	// There are multiple sorting strategies, each suited for different scenarios
	// 1. Sort input records. (current approach) This has the benefit that:
	//    + continuous ID check is as easy as keeping track of the previous ID,
	//      or alternatively it's just the current element index.
	//    + parent node is always created prior to children, making children-attaching
	//      a bit easier.
	// 2. Build the tree first regardless ordering and sort afterwards.
	//    this might be more desirable if number of records are huge,
	//    as this `parent ID - children ID` relation allows breaking large chunk of data
	//    into smaller pieces, so a general sorting algorithm can work on smaller chunks
	//    and thus more performant. But this approach has the disadvantage that, since
	//    we don't assume order on input records, a parent might be missing when we get to
	//    a children, so we might need an auxilary function to do "find this node by id,
	//    or create one if missing" thingy.
	// 3. Build the tree while maintaining sorted order of children.
	//    this is the approach I would prefer if there is no "continuous id requirement"
	//    (I would suspect in a real world setting such a requirement is far away from
	//    practical)
	//    This is similar to 2., but we can build up sorted children taking advantage
	//    of binary search. However the implementation will be a bit more complicated
	//    as we need to branch on binary search results and piece together slices.
	sort.Slice(records, func(i, j int) bool { return records[i].ID < records[j].ID })

	var root *Node
	m := make(map[int]*Node)

	for i, r := range records {
		if r.ID != i {
			return nil, ErrIDNotContinuous
		}

		curNode := Node{ID: r.ID}
		m[r.ID] = &curNode
		if r.ID == r.Parent {
			if root != nil {
				return nil, ErrRootNotUnique
			}
			root = &curNode
		} else {
			if r.ID < r.Parent {
				return nil, ErrParentIDHigher
			}
			// This lookup is safe due to the fact that input record is sorted,
			// therefore parent node always exist when creating children.
			parentNode := m[r.Parent]
			parentNode.Children = append(parentNode.Children, &curNode)
		}
	}
	return root, nil
}
