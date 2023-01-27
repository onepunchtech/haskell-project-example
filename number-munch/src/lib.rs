use std::slice;
use std::ptr;
use std::mem;

use ta::indicators::SimpleMovingAverage;
use ta::Next;

#[no_mangle]
pub extern "C" fn simple_moving_average(k: u32,
                                        v_ptr: *const f64,
                                        v_len: u32,
                                        res_len: *mut u32,
                                        ) -> *mut f64 {
    let numbers = unsafe {
        assert!(!v_ptr.is_null());

        slice::from_raw_parts(v_ptr, v_len as usize)
    };


    let mut sma = SimpleMovingAverage::new(k as usize).unwrap();
    let mut res: Vec<f64> = Vec::new();

    for i in numbers {
        res.push(sma.next(*i))
    }

    res.shrink_to_fit();
    assert!(res.len() == res.capacity());
    unsafe {
        *res_len = res.len() as u32;
    }
    let res_ptr = res.as_mut_ptr();
    mem::forget(res);

    return res_ptr
}

#[no_mangle]
pub extern "C" fn mean(v_ptr: *const f64, v_len: u32) -> f64 {
    if v_len == 0 {
        return 0.0
    }

    let numbers = unsafe {
        assert!(!v_ptr.is_null());

        slice::from_raw_parts(v_ptr, v_len as usize)
    };

    let mut sum = 0.0;

    for n in numbers {
        sum += n
    }


    return sum / v_len as f64
}
