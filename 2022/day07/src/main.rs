use std::{cell::RefCell, fs, rc::Rc};

#[derive(Debug)]
struct DirInfo {
    parent: Option<Rc<RefCell<DirInfo>>>,
    name: String,
    dirs: Vec<Rc<RefCell<DirInfo>>>,
    files: Vec<FileInfo>,
    total_size: u32,
}

#[derive(Debug)]
struct FileInfo {
    name: String,
    size: u32,
}

impl DirInfo {
    fn root(name: &str) -> DirInfo {
        DirInfo {
            name: name.to_string(),
            dirs: Vec::new(),
            files: Vec::new(),
            total_size: 0,
            parent: None,
        }
    }

    fn add_file(&mut self, name: &str, size: u32) -> bool {
        if self.files.iter().any(|f| f.name == name) {
            return false;
        }

        self.files.push(FileInfo {
            name: name.to_string(),
            size,
        });
        self.inc_to_root(size);
        return true;
    }

    fn inc_to_root(&mut self, size: u32) {
        self.total_size += size;
        match &self.parent {
            None => return,
            Some(p) => p.borrow_mut().inc_to_root(size),
        }
    }

    fn add_dir(self_: Rc<RefCell<DirInfo>>, name: &str) -> Rc<RefCell<DirInfo>> {
        assert!(!self_.borrow().dirs.iter().any(|d| d.borrow().name == name));
        let dir = DirInfo {
            name: name.to_string(),
            total_size: 0,
            files: Vec::new(),
            dirs: Vec::new(),
            parent: Some(Rc::clone(&self_)),
        };
        let new_dir_ref = Rc::new(RefCell::new(dir));
        self_.borrow_mut().dirs.push(Rc::clone(&new_dir_ref));
        new_dir_ref
    }

    fn fold<F, Arg>(self_: Rc<RefCell<DirInfo>>, seed: Arg, f: F) -> Arg
    where
        F: Fn(Arg, Rc<RefCell<DirInfo>>) -> Arg,
    {
        let mut current = seed;
        let mut queue = Vec::new();
        queue.push(Rc::clone(&self_));

        while !queue.is_empty() {
            let dir = queue.pop().unwrap();
            for child in &dir.borrow().dirs {
                queue.push(Rc::clone(&child));
            }
            current = f(current, Rc::clone(&dir));
        }

        return current;
    }
}

fn main() {
    let root = DirInfo::root("/");
    let root_ref = Rc::new(RefCell::new(root));
    let mut current_dir = Rc::clone(&root_ref);
    let input = fs::read_to_string("input.txt").unwrap();

    let mut lines = input.lines();
    let mut is_listing = false;
    while let Some(cmd) = lines.next() {
        if cmd.starts_with("$ cd") {
            is_listing = false;

            let dir_path = &cmd["$ cd ".len()..];
            current_dir = match dir_path {
                "/" => root_ref.clone(),
                ".." => current_dir
                    .borrow()
                    .parent
                    .clone()
                    .expect("Current dir should have a parent"),
                _ => match current_dir
                    .borrow()
                    .dirs
                    .iter()
                    .find(|d| d.borrow().name == dir_path)
                {
                    None => DirInfo::add_dir(Rc::clone(&current_dir), dir_path),
                    Some(dir) => Rc::clone(dir),
                },
            }
        } else if cmd.starts_with("$ ls") {
            is_listing = true;
        } else if is_listing {
            let mut parts = cmd.split(" ");
            let part1 = parts.next().expect("Should have 1st part");
            let name = parts.next().expect("Should find second part");
            if part1 == "dir" {
                DirInfo::add_dir(Rc::clone(&current_dir), name);
            } else {
                let size = part1.parse::<u32>().expect("Should parse size");
                current_dir.borrow_mut().add_file(name, size);
            }

            assert!(parts.next().is_none());
        } else {
            panic!("Unknown command {}", cmd);
        }
    }

    let part1 = DirInfo::fold(Rc::clone(&root_ref), 0, |acc, d| {
        if d.borrow().total_size <= 100000 {
            return acc + d.borrow().total_size;
        }
        return acc;
    });
    println!("part1: {}", part1); // 1453349

    let unused_now = 70000000 - root_ref.borrow().total_size;
    let need_to_free = 30000000 - unused_now;
    println!("need_to_free: {}", need_to_free);

    let part2 = DirInfo::fold(Rc::clone(&root_ref), None, |min_dir_opt, dir| {
        let size = dir.borrow().total_size;
        if size >= need_to_free {
            return match min_dir_opt {
                None => Some(Rc::clone(&dir)),
                Some(min_dir) => {
                    if size < min_dir.borrow().total_size {
                        return Some(Rc::clone(&dir));
                    }

                    return Some(Rc::clone(&min_dir));
                }
            };
        }

        return min_dir_opt;
    })
    .unwrap()
    .borrow()
    .total_size;

    println!("part2: {}", part2); // 2948823
}
