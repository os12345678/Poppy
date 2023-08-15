; ModuleID = 'core_lib/bindings.c'
source_filename = "core_lib/bindings.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx13.0.0"

%struct._opaque_pthread_t = type { i64, %struct.__darwin_pthread_handler_rec*, [8176 x i8] }
%struct.__darwin_pthread_handler_rec = type { void (i8*)*, i8*, %struct.__darwin_pthread_handler_rec* }
%struct._opaque_pthread_attr_t = type { i64, [56 x i8] }

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @print(i8* %0, i64* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i64*, align 8
  store i8* %0, i8** %3, align 8
  store i64* %1, i64** %4, align 8
  %5 = load i8*, i8** %3, align 8
  %6 = load i64*, i64** %4, align 8
  %7 = load i64, i64* %6, align 8
  %8 = call i32 (i8*, ...) @printf(i8* %5, i64 %7)
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct._opaque_pthread_t* @create_thread(i8* (i8*)* %0, i8* %1) #0 {
  %3 = alloca i8* (i8*)*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca %struct._opaque_pthread_t*, align 8
  store i8* (i8*)* %0, i8* (i8*)** %3, align 8
  store i8* %1, i8** %4, align 8
  %6 = load i8* (i8*)*, i8* (i8*)** %3, align 8
  %7 = load i8*, i8** %4, align 8
  %8 = call i32 @pthread_create(%struct._opaque_pthread_t** %5, %struct._opaque_pthread_attr_t* null, i8* (i8*)* %6, i8* %7)
  %9 = load %struct._opaque_pthread_t*, %struct._opaque_pthread_t** %5, align 8
  ret %struct._opaque_pthread_t* %9
}

declare i32 @pthread_create(%struct._opaque_pthread_t**, %struct._opaque_pthread_attr_t*, i8* (i8*)*, i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @join_thread(%struct._opaque_pthread_t* %0) #0 {
  %2 = alloca %struct._opaque_pthread_t*, align 8
  store %struct._opaque_pthread_t* %0, %struct._opaque_pthread_t** %2, align 8
  %3 = load %struct._opaque_pthread_t*, %struct._opaque_pthread_t** %2, align 8
  %4 = call i32 @"\01_pthread_join"(%struct._opaque_pthread_t* %3, i8** null)
  ret i32 %4
}

declare i32 @"\01_pthread_join"(%struct._opaque_pthread_t*, i8**) #1

attributes #0 = { noinline nounwind optnone ssp uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #1 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 13, i32 1]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"branch-target-enforcement", i32 0}
!3 = !{i32 1, !"sign-return-address", i32 0}
!4 = !{i32 1, !"sign-return-address-all", i32 0}
!5 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 1}
!8 = !{i32 7, !"frame-pointer", i32 1}
!9 = !{!"Apple clang version 14.0.0 (clang-1400.0.29.202)"}
