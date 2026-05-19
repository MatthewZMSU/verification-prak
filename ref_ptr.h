#ifndef BUCHI_REFPTR_H
#define BUCHI_REFPTR_H

template<typename T>
class ref_ptr {
public:
    using pointer_type = T*;
    using element_type = T;

    ref_ptr(pointer_type p = nullptr) {
        reset(p);
    }

    ref_ptr(const ref_ptr &that) {
        reset(that.get());
    }

    ref_ptr(ref_ptr &&that) {
        reset(that.release(), false);
    }

    template<typename U>
    ref_ptr(const ref_ptr<U> &that) {
        reset(that.get());
    }

    template<typename U>
    ref_ptr(ref_ptr<U> &&that) {
        reset(that.release(), false);
    }

    ~ref_ptr() {
        if (get()) {
            ref_ptr_release(*get());
        }
    }

    pointer_type get() const {
        return ptr;
    }

    pointer_type release() {
        pointer_type p = get();
        ptr = nullptr;
        return p;
    }

    void reset(pointer_type p) {
        reset(p, true);
    }

    void reset(pointer_type p, bool inc) {
        if (p && inc) {
            ref_ptr_inc_ref(*p);
        }
        if (ptr) {
            ref_ptr_release(*ptr);
        }
        ptr = p;
    }

    ref_ptr &operator=(const ref_ptr &that) {
        reset(that.get());
        return *this;
    }

    ref_ptr &operator=(ref_ptr &&that) {
        reset(that.release(), false);
        return *this;
    }

    template<typename U>
    ref_ptr &operator=(const ref_ptr<U> &that) {
        reset(that.get());
        return *this;
    }

    template<typename U>
    ref_ptr &operator=(ref_ptr<U> &&that) {
        reset(that.release());
        return *this;
    }

    ref_ptr &operator=(T *p) {
        reset(p);
        return *this;
    }

    T *operator->() const {
        return ptr;
    }

    T &operator*() const {
        return *ptr;
    }

    explicit operator bool() const {
        return ptr != 0;
    }

private:
    pointer_type ptr = nullptr;
};

#endif
