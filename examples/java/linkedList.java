class LinkedListNode {
    int data;
    LinkedListNode next;

    public LinkedListNode(int data) {
        this.data = data;
        this.next = null;
    }
}

class LinkedList {
    private LinkedListNode head;

    public LinkedList() {
        this.head = null;
    }

    // Method to add a node to the front of the list
    public void addFirst(int data) {
        LinkedListNode newNode = new LinkedListNode(data);
        newNode.next = head;
        head = newNode;
    }

    // Method to add a node to the end of the list
    public void addLast(int data) {
        LinkedListNode newNode = new LinkedListNode(data);
        if (head == null) {
            head = newNode;
        } else {
            LinkedListNode current = head;
            while (current.next != null) {
                current = current.next;
            }
            current.next = newNode;
        }
    }

    // Method to print the list
    public void printList() {
        LinkedListNode current = head;
        while (current != null) {
            System.out.print(current.data + " -> ");
            current = current.next;
        }
        System.out.println("null");
    }

    // Method to remove the first element from the list
    public void removeFirst() {
        if (head != null) {
            head = head.next;
        }
    }

    // Method to remove the last element from the list
    public void removeLast() {
        if (head == null) return;

        if (head.next == null) {
            head = null;
        } else {
            LinkedListNode current = head;
            while (current.next.next != null) {
                current = current.next;
            }
            current.next = null;
        }
    }
    
    // Main method to demonstrate the functionality
    public static void main(String[] args) {
        LinkedList list = new LinkedList();
        
        list.addFirst(3);
        list.addFirst(2);
        list.addFirst(1);
        list.addLast(4);
        
        list.printList(); // Output: 1 -> 2 -> 3 -> 4 -> null
        
        list.removeFirst();
        list.printList(); // Output: 2 -> 3 -> 4 -> null
        
        list.removeLast();
        list.printList(); // Output: 2 -> 3 -> null
    }
}
