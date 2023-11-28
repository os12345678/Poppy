struct ListNode {
    data: i32,
    next: Option<Box<ListNode>>,
}

struct LinkedList {
    head: Option<Box<ListNode>>,
}

impl ListNode {
    fn new(data: i32) -> Self {
        ListNode { data, next: None }
    }
}

impl LinkedList {
    fn new() -> Self {
        LinkedList { head: None }
    }

    // Method to add a node to the front of the list
    fn add_first(&mut self, data: i32) {
        let mut new_node = Box::new(ListNode::new(data));
        new_node.next = self.head.take();
        self.head = Some(new_node);
    }

    // Method to add a node to the end of the list
    fn add_last(&mut self, data: i32) {
        let new_node = Box::new(ListNode::new(data));
        if let Some(ref mut current) = self.head {
            let mut tail = current;
            while let Some(ref mut next) = tail.next {
                tail = next;
            }
            tail.next = Some(new_node);
        } else {
            self.head = Some(new_node);
        }
    }


    // Method to print the list
    fn print_list(&self) {
        let mut current = &self.head;
        while let Some(ref node) = current {
            print!("{} -> ", node.data);
            current = &node.next;
        }
        println!("null");
    }

    // Method to remove the first element from the list
    fn remove_first(&mut self) {
        if self.head.is_some() {
            let next = self.head.as_mut().unwrap().next.take();
            self.head = next;
        }
    }

    // Method to remove the last element from the list
    fn remove_last(&mut self) {
        if self.head.is_none() {
            return;
        }
        if self.head.as_ref().unwrap().next.is_none() {
            self.head.take();
            return;
        }

        let mut current = &mut self.head;
        while let Some(ref mut next_node) = current.as_mut().unwrap().next {
            if next_node.next.is_none() {
                break;
            }
            current = &mut current.as_mut().unwrap().next;
        }
        current.as_mut().unwrap().next.take();
    }
}

fn main() {
    let mut list = LinkedList::new();
    
    list.add_first(3);
    list.add_first(2);
    list.add_first(1);
    list.add_last(4);
    
    list.print_list(); // Output: 1 -> 2 -> 3 -> 4 -> null
    
    list.remove_first();
    list.print_list(); // Output: 2 -> 3 -> 4 -> null
    
    list.remove_last();
    list.print_list(); // Output: 2 -> 3 -> null
}
